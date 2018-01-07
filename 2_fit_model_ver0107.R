##############################
#
#Null Model
#
#############################
#Unbalance Resonse
train %>%
  ggplot(aes(as.character(is_churn))) +
  geom_bar(fill = c('#121212','#676755'), alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..),vjust = -0.5) +
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold") )  +
  labs(x = "is_churn")
( ratio <- 7450/550 )
#[1] 13.54545

#use as ln(π /1−π) = beta0(intercept) as null model 
m_null_intercept <- glm(is_churn ~ 1 , data = train, family=binomial(link="logit"))
pred_vector <- ifelse(predict(m_null,newdata=test)>0.5, 1, 0 )
table(pred_vector)
perf(pred_vector)
#    pred_vector
#true    0
#0      1878
#1      122

#maually set null model
set.seed(123)
ramdom_value <- runif(n = 8000, min = 0, max = 7450)
pred_null_train  <- ifelse(ramdom_value>550, 0, 1)
table(pred_null_train)
perf(pred_null_train)

ramdom_value <- runif(n = 2000, min = 0, max = 1847/153)
pred_null_test  <- ifelse(ramdom_value>1, 0, 1)
table(pred_null_test) 
perf(pred_null_test )
m_null_2000 <- glm(is_churn ~ pred_null_test  , data = test, family=binomial(link="logit"))


ramdom_value <- runif(n = 8000, min = 0, max = 7450)
pred_null_train  <- ifelse(ramdom_value>550, 0, 1)
m_null_8000 <- glm(is_churn ~ pred_null_train  , data = train, family=binomial(link="logit"))

##############################
#
# logistic regression
#
#############################
m_logit <- glm(is_churn ~ . , data = train, family=binomial(link="logit"))

#MFadden’s pseudo-R
1- as.vector(logLik(m_logit)/logLik(m_null)) 

#performance
pred_logit <- ifelse(predict.glm(m_logit, newdata=train, type = "response" )>0.5, 1, 0 )
 perf(pred_logit)

##############################
#
# begging logistic regression
#
#############################
#install.packages('foreach')
library(foreach)
set.seed(123)
length <- sum(train$is_churn == 1)
iterations     <- floor( nrow(train) / length )

pred_bag_temp <- foreach(m=1:iterations,.combine=rbind) %do% {
                  idx <- sample(sum(train$is_churn == 1)+1 :sum(train$is_churn == 0), size=length)
                  idx <- c(c(1:sum(train$is_churn == 1)), idx)
                  m_temp <- glm(is_churn ~ . ,
                                 data=train[ idx,],
                                 family=binomial(logit))
                  #predict
                  pred_vector <- ifelse(predict.glm(m_temp,newdata=train, type = "response" )>0.5, 1, 0 )
                  return( pred_vector )
              }
pred_bag <- as.data.frame( t( pred_bag_temp ) ) %>%
  apply(1, max ) 

#performance
table(pred_bag)

m_beg_8000 <- glm(is_churn ~ pred_bag  , data = train, family=binomial(link="logit"))


#Ramdom forests
library(randomForest)
set.seed(123)
x_rf <- train
x_rf$is_churn <- as.factor(x_rf$is_churn)
m_f <- randomForest(is_churn ~ ., ntree=10, nodesize=6, mtry=9 , data = x_rf ,  importance=T )
pred_f <- predict(m_f, newdata=train)

##############################
#
# performance
#
#############################

perf(m_null_pred_vector_2000)
ifelse(predict(m_logit,newdata=test)>0.5, 1, 0 ) %>% perf()
perf(pred_bag)

perf <- function( pred_vector ){
  t1 <- table(true=test$is_churn, pred_vector)
  print(t1)
    accuracy =  (t1[1,1]+t1[2,2])/sum(t1) 
    precision = t1[2,2]/(t1[2,2]+t1[1,2]) #TP/true churn(Lost) diagnose
    recall =  t1[2,2]/(t1[2,2]+t1[2,1])  #TP/true churn(Lost)  appear
    F1Measure = (precision * recall * 2) / (precision + recall)
  performance <- round(c(accuracy=accuracy, precision=precision, recall=recall, F1Measure=F1Measure),3)
  print(performance)
}




