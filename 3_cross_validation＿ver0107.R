
##################
#
# model functions
#
##################

m_null <- function(x,y){
  ramdom_value <- runif(n = nrow(x), min = 0, max = 7450)
  train  <- ifelse(ramdom_value>550, 0, 1)
  ramdom_value <- runif(n = nrow(y), min = 0, max = 7450)
  predict  <- ifelse(ramdom_value>550, 0, 1)
  return(list(train=train, predict=predict))
}

m_logit <- function(x,y){
  m_logit <- glm(is_churn ~ . , data = x, family=binomial(link="logit"))
  train <- ifelse(predict.glm(m_logit, newdata=x, type = "response" )>0.5, 1, 0 )
  predict  <- ifelse(predict.glm(m_logit, newdata=y, type = "response" )>0.5, 1, 0 )
  return(list(train=train, predict=predict))
}

m_bag <- function(x,y){
  library(foreach)
  set.seed(123)
  length <- sum(x$is_churn == 1)
  iterations     <- floor( nrow(x) / length )
  
  pred_bag_temp <- foreach(m=1:iterations,.combine=rbind) %do% {
    idx_1 <- sample(sum(x$is_churn == 1), size=length)
    idx_0 <- sample(sum(x$is_churn == 0), size=length)
    idx <- c(idx_1, idx_0)
    glm_fit <- glm(is_churn ~ . ,
                   data=x[ idx,],
                   family=binomial(logit))
    #predict all the taining data
    pred_vector_1 <- ifelse(predict.glm(glm_fit,newdata=x, type = "response" )>0.5, 1, 0 )
    #predict all the calib/testing data
    pred_vector_2 <- ifelse(predict.glm(glm_fit,newdata=y, type = "response" )>0.5, 1, 0 )
    return( list(pred_vector_1, pred_vector_2) )
  }
  train <- as.data.frame(  pred_bag_temp[,1]  ) %>%apply(1, max ) 
  predict <- as.data.frame(  pred_bag_temp[,2]  ) %>%apply(1, max ) 
  return(list(train=train, predict=predict))
}

m_rf <-  function(x,y){
  library(randomForest)
  set.seed(123)
  x_rf <- x 
  x_rf$is_churn <- as.factor(x_rf$is_churn)
  fmodel <- randomForest(is_churn ~ ., ntree=10, nodesize=6, mtry=9 , data = x_rf ,  importance=T )
  train <- predict(fmodel, newdata=x_rf)
  predict  <- predict(fmodel, newdata=y)
  return(list(train=train, predict=predict))
}

performance <- function( x,y ){
  t1 <- table(true=y$is_churn, pred=x)
  #print(t1)
  accuracy =  (t1[1,1]+t1[2,2])/sum(t1) 
  precision = t1[2,2]/(t1[2,2]+t1[1,2]) #TP/true churn(Lost) diagnose
  recall =  t1[2,2]/(t1[2,2]+t1[2,1])  #TP/true churn(Lost)  appear
  F1Measure = (precision * recall * 2) / (precision + recall)
  performance <- round(c(accuracy=accuracy, precision=precision, recall=recall, F1Measure=F1Measure),3)
  return(performance)
}

##################
#
# create folds  
#
##################
#install.packages('caret')
require(caret) 

fold = 5 #set how many folds
d<-rbind(train,test)

#create segment index
set.seed(12345)
flds <- createFolds( y=d$is_churn, k = fold, list = TRUE, returnTrain = FALSE)
#call the function to choose ‘the fold‘
nfcv<-function(i){
  d_test<-d[flds[[i]],]
  if(i+1<=fold){
    d_calib<-d[flds[[i+1]],]
  }else{
    d_calib<-d[flds[[i+1-fold]],]
  }
  if(i+2<=fold){
    d_train<-d[flds[[i+2]],]
  }else{
    d_train<-d[flds[[i+2-fold]],]
  }
  for (j in  1:(fold-3) ){
    if(i+3<=fold){
      d_train<-rbind(d_train,d[flds[[i+3]],])
    }else{
      d_train<-rbind(d_train,d[flds[[i+3-fold]],])
    }
  }
  return( list(d_test,d_calib,d_train) )
}

##################
#
# fit all the model
#
##################

#create a empty list to put performance
perf_summary <- list()

# run k-fold 
for (fold_i in  1:fold ){
  
  data<-nfcv(fold_i)
  d_train<-data[[3]]
  d_calib<-data[[2]]
  d_test<-data[[1]]
  
  #null model
  pred_temp <- m_null(d_train, d_calib)
  perf_summary$null_train <- rbind( perf_summary$null_train, performance(pred_temp$train, d_train) )
  perf_summary$null_calib <- rbind( perf_summary$null_calib, performance(pred_temp$predict, d_calib) )
  
  #logistic regression
  pred_temp <- m_logit(d_train, d_calib)
  perf_summary$logit_train <- rbind( perf_summary$logit_train, performance(pred_temp$train, d_train) )
  perf_summary$logit_calib <- rbind( perf_summary$logit_calib, performance(pred_temp$predict, d_calib) )
  
  #begging logistic regression
  pred_temp <- m_bag(d_train, d_calib)
  perf_summary$bag_train <- rbind( perf_summary$bag_train, performance(pred_temp$train, d_train) )
  perf_summary$bag_calib <- rbind( perf_summary$bag_calib, performance(pred_temp$predict, d_calib) )
  
  #Ramdom forests
  pred_temp <- m_rf(d_train, d_calib)
  perf_summary$rf_train <- rbind( perf_summary$rf_train, performance(pred_temp$train, d_train) )
  perf_summary$rf_calib <- rbind( perf_summary$rf_calib, performance(pred_temp$predict, d_calib) )
  
  #choose the best model for test
  ps_temp <- c()
  for(ps_i in seq(2,8,by=2)){ ps_temp = c(ps_temp,perf_summary[[ps_i]][fold_i,4] )}
  print( which.max(ps_temp) )
  switch( which.max(ps_temp),
          pred_temp <- m_null(d_train, d_calib),
          pred_temp <- m_logit(d_train, d_calib),
          pred_temp <- m_bag(d_train, d_calib),
          pred_temp <- m_rf(d_train, d_calib)
        )
  perf_summary$test <- rbind( perf_summary$test, performance(pred_temp$predict, d_calib ))
  
  }
#######################
#
# visualize performance
#
#######################
{
  #mean of performance in k folds
  perf_summary_mean <- data.frame()
  for(i in 1:length(perf_summary)){
    perf_summary_mean <- rbind(perf_summary_mean, apply(perf_summary[[i]], 2, mean) )
  }
  perf_summary_mean <- cbind( perf_summary_mean,names(perf_summary) ) 
  colnames(perf_summary_mean) <- c('accuracy' , 'precision' , 'recall' , 'F1Measure', 'model' )
  perf_summary_mean <- cbind(model=perf_summary_mean$model, perf_summary_mean[,1:4])
  colnames(perf_summary_mean)[1] <-'model' 
  print( perf_summary_mean )
  #plot
  library(reshape2)
  df<-melt(perf_summary_mean,id='model')
  df$model <- factor(df$model ,levels = names(perf_summary))
  ggplot(df, aes(fill=variable, y=value, x=model, label=round(value,2))) +
    geom_bar(position="dodge",stat="identity") +
    theme(legend.position = "bottom", axis.text = element_text(size = 18), 
            axis.title = element_text(size = 0)) +
    geom_text(aes(group=variable),position=position_dodge(width=0.9),size=4.3,vjust=-0.5,hjust=0.5) 
}




















