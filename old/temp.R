
#visualization
ul_s_c %>%
ggplot(aes(total_secs)) +
geom_density(fill = "orange", alpha = 0.6) +
scale_x_log10()


############
#
# Fit Model
#
############
#fit logit & probit regression models
m1 <- glm(is_churn ~ . , data = train, family=binomial(link="logit"))
m2 <- glm(is_churn ~ . , data = train, family=binomial(link="probit"))

############
#
# Null Model
#
############
#calculate the MacFadden’s pseudo-R
m1_0 <- update(m1, formula = is_churn ~ 0) #what are we doing here?
1- as.vector(logLik(m1)/logLik(m1_0)) 
m2_0 <- update(m2, formula = is_churn ~ 0) #what are we doing here?
1- as.vector(logLik(m2)/logLik(m2_0)) 

#anova test
anova(m1, m1_0, test= "Chisq") 
anova(m2, m2_0, test= "Chisq") 

#calculate the MacFadden’s pseudo-R
m1_1 <- update(m1, formula = is_churn ~ 1) #what are we doing here?
1- as.vector(logLik(m1)/logLik(m1_1)) 
m2_1 <- update(m2, formula = is_churn ~ 1) #what are we doing here?
1- as.vector(logLik(m2)/logLik(m2_1)) 

#anova test
anova(m1, m1_1, test= "Chisq") 
anova(m2, m2_1, test= "Chisq") 

############
#
# performance
#
############

#comfusing matrix
pred1 <- ifelse(predict.glm(m1,newdata=test, type = "response")>0.5,1,0 )
t1 <- table(true=test$is_churn, pred1)
t1
sum(diag(t1))/sum(t1)

pred2 <- ifelse(predict.glm(m1,newdata=test, type = "response")>0.5,1,0 )
t2 <- table(true=test$is_churn, pred2)
t2
sum(diag(t2))/sum(t2)


library(ROCR)
pred1 <- predict.glm(m1,newdata=test)
pred=prediction(pred1, test$is_churn)
par(mfrow=c(1,2))
plot(performance(pred,"tpr","fpr"))
abline(0,1,lty=2)

#search the best threshold
for( i in seq(-4,4,by=0.1)){
  pred1 <- ifelse(predict.glm(m_beg,newdata=test)>i, 1, 0 )
  t1 <- table(true=test$is_churn, pred1)
  print(paste('i=',i))
  performance = c(
    accuracy =  (t1[1,1]+t1[2,2])/sum(t1), 
    precision = t1[1,1]/(t1[1,1]+t1[2,1]), 
    recall =  t1[1,1]/(t1[1,1]+t1[1,2]), 
    F1Measure = (2*precision*recall) / (precision+recall)
  )
  print(performance)
  
}

############
#
# others
#
############

#共線性
vif(m1, digits = 3)

#看關聯圖
plot(joint1[, 2:17])

############
#
# p-value & R2
#
############
delDev <- m_null$deviance - m_logit$deviance
deldf <- m_null$df.residual -m_logit$df.residual
pchisq(delDev, deldf, lower.tail=F)

m_null_8000
m_logit
m_beg_8000
m_f
pred_null_train
pred_logit
pred_bag
pred_f <- as.numeric(as.character(pred_f))
#McFadden’s pseudo-R-squared
1- as.vector(logLik(m_logit)/logLik(m_null_8000))
1- as.vector(logLik(m_beg_8000)/logLik(m_null_8000))
1- as.vector(logLik(m_logit))/logLik(m_null_intercept)
1- as.vector(logLik(m_beg_8000))/logLik(m_null_intercept)
1- as.vector(logLik(m_f))/logLik(m_null_intercept)

actual <- pred_null_train
predicted <-pred_bag
1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
caret::RMSE(predicted,actual)

#f test
anova(m_null_intercept, m_logit, test = "Chisq")
anova(m_null_8000, m_logit, test = "Chisq")
anova(m_null_8000, m_beg_8000, test = "Chisq")
anova(m_null_8000, m_f, test = "Chisq")


anova(m_null_intercept, m_logit, test = "Chisq")
anova(m_null_intercept, m_beg_8000, test = "Chisq")
anova(m_null_intercept, m_f, test = "Chisq")


#Calculating the significance of the observed fit #week14
df.null <- dim(train)[[1]] - 1
df.model <- dim(train)[[1]] - length(model$coefficients)
delDev <- null.dev - resid.dev
deldf <- df.null - df.model
p <- pchisq(delDev, deldf, lower.tail=F)

#The pseudo R-squared
1 - (m_logit$deviance/m_null_8000$deviance)
1 - (m_beg_8000$deviance/m_null_8000$deviance)
