#待做：看一下 m_beg 的 R^2
#待做：m_beg還是不夠準，可以考慮將顯著的係數來平均就好

#Null Model
m_null <- glm(is_churn ~ 1 , data = train, family=binomial(link="logit"))

#logistic regression
m_logit <- glm(is_churn ~ . , data = train, family=binomial(link="logit"))

#MacFadden’s pseudo-R
1- as.vector(logLik(m_logit)/logLik(m_null)) 

#begging logistic regression
#install.packages('foreach')
library(foreach)
set.seed(123)
length <- sum(train$is_churn == 1)
iterations     <- floor( nrow(train) / length )

pred_coef <- foreach(m=1:iterations,.combine=rbind) %do% {
                  idx <- sample(sum(train$is_churn == 1)+1 :sum(train$is_churn == 0), size=length)
                  idx <- c(c(1:sum(train$is_churn == 1)), idx)
                  glm_fit <- glm(is_churn ~ . ,
                                 data=train[idx,],
                                 family=binomial(logit))
                  summary <- summary(glm_fit)
                  return( summary$coeff[,1] )
              }
mean_pred_coef <- apply(pred_coef, 2, mean )
#修改模型係數
m_beg <- glm_fit
m_beg$coefficients <- mean_pred_coef
#MacFadden’s pseudo-R
1- as.vector(logLik(m_beg)/logLik(m_null)) 

#CM
pred1 <- ifelse(predict.glm(m_beg,newdata=test, type = "response")>0.5,1,0 )
plot(predict.glm(m_beg,newdata=test))
t1 <- table(true=test$is_churn, pred1)
t1
sum(diag(t1))/sum(t1)



train %>%
  ggplot(aes(as.character(is_churn))) +
  geom_bar(fill = c('#121212','#676755'), alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..),vjust = -0.5) +
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold") )  +
  labs(x = "is_churn")









