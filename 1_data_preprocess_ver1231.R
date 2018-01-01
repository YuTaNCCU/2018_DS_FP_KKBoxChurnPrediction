
library(dplyr)
library(data.table)
library(ggplot2)
library(car)

setwd("~/Desktop/DS_KKBox")
#quickly import the prcocessed data
tr_s <- fread('data/tr_s.csv', sep = ",", header=T, stringsAsFactors = T)
mb_s <- fread('data/mb_s.csv', sep = ",", header=T, stringsAsFactors = T)
ts_s <- fread('data/ts_s.csv', sep = ",", header=T, stringsAsFactors = T)
ul_s <- fread('data/ul_s.csv', sep = ",", header=T, stringsAsFactors = T)
joint1 <- fread('data/joint1.csv', sep = ",", header=T, stringsAsFactors = T)
train <- fread('data/train.csv', sep = ",", header=T, stringsAsFactors = T)
test <- fread('data/test.csv', sep = ",", header=T, stringsAsFactors = T)

##################
#
# A: Data process
#
##################

#readfile
tr <- fread('data/train.csv', sep = ",", header=T, stringsAsFactors = T)
mb <- fread('data/members.csv', sep = ",", header=T, stringsAsFactors = T)
ts <- fread('data/transactions.csv', sep = ",", header=T, stringsAsFactors = T)
ul <- fread('data/user_logs.csv', sep = ",", header=T, stringsAsFactors = T)

#join the user ID (msno) to make sure all the members are in each dataframe
sp_idx_1<-tr %>% select(msno)
sp_idx_2<-mb %>% filter( msno %in% sp_idx_1$msno) %>% group_by(msno) %>% summarise()
sp_idx_3<-ts %>% filter( msno %in% sp_idx_2$msno) %>% group_by(msno) %>% summarise()
sp_idx_4<-ul %>% filter( msno %in% sp_idx_3$msno) %>% group_by(msno) %>% summarise()
#sampling 10,000 members from 1,000,000 members
set.seed(123)
sample_msno <- sample(sp_idx_4$msno, size=10000)
tr_s<-tr[tr$msno %in% sample_msno ,]
mb_s<-mb[mb$msno %in% sample_msno ,]
ts_s<-ts[ts$msno %in% sample_msno ,]
ul_s<-ul[ul$msno %in% sample_msno ,]
#make sure all the 10,000 members are all in the dataframe
length(unique(tr_s$msno))
length(unique(mb_s$msno))
length(unique(ts_s$msno))
length(unique(ul_s$msno))
#write the sampled data into files in order not to read the original files again
fwrite(tr_s, file = 'data/tr_s.csv', append = FALSE, quote = "auto")
fwrite(mb_s, file = 'data/mb_s.csv', append = FALSE, quote = "auto")
fwrite(ts_s, file = 'data/ts_s.csv', append = FALSE, quote = "auto")
fwrite(ul_s, file = 'data/ul_s.csv', append = FALSE, quote = "auto")

#Descriptive Statistics
glimpse(tr_s)
summary(tr_s)
glimpse(mb_s)
summary(mb_s)
glimpse(ts_s)
summary(ts_s)
glimpse(ul_s)
summary(ul_s)

#clean the missing values and the outliers
ul_s_c <- ul_s %>%  #ul_s$total_secs has many huge values, so I modified the rows
  mutate(total_secs = ifelse(ul_s$total_secs <=0 , 0.000001,  #not 0, for scaling reason 
                             ifelse(ul_s$total_secs > 86400, 86400, ul_s$total_secs ) ) )
mb_s_c <- mb_s %>%  #3 col have many 0 ,and reg_init_time is meaningless, so I delete them
  select( -c(bd, gender, expiration_date, registration_init_time) )
fwrite(mb_s_c, file = 'data/mb_s_c.csv', append = FALSE, quote = "auto")
fwrite(ul_s_c, file = 'data/ul_s_c.csv', append = FALSE, quote = "auto")

#choode and create features
ts_s_f <- ts_s %>%
  group_by(msno) %>%
  summarise(
    mean_pay = round( mean(actual_amount_paid) ,2),
    sum_pay = round( sum(actual_amount_paid) ,2),
    count_trans = length( transaction_date [is_cancel==0 ] ), #交易次數，扣除掉該次交易是取消訂閱的
    count_cancel = length( is_cancel[is_cancel==1 ] ),  #曾經取消的次數
    mean_duration = round( mean(payment_plan_days) ,2),     #平均訂閱的天數
    total_trans_period =  max(sapply(membership_expire_date, #交易的總時間 （ 最後一次交易日期 - 第一次交易日期）
                               function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" ))) -
                    min(sapply(transaction_date, 
                               function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" )))
  )
ul_s_c_f <- ul_s_c %>%
  group_by(msno) %>%
  summarise(
    mean_num_25 = round( mean(num_25) ,2),
    mean_num_50 = round( mean(num_50) ,2),
    mean_num_75 = round( mean(num_75) ,2),
    mean_num_985 = round( mean(num_985) ,2),
    mean_num_100 = round( mean(num_100) ,2),
    mean_num_unq = round( mean(num_unq) ,2),
    mean_total_secs = round( mean(total_secs) ,2),
    count_date = n()
  )
fwrite(ts_s_f, file = 'data/ts_s_f.csv', append = FALSE, quote = "auto")
fwrite(ul_s_c_f, file = 'data/ul_s_c_f.csv', append = FALSE, quote = "auto")

#join the data set
joint1 <- tr_s %>% 
  left_join(mb_s_c, by='msno') %>%
  left_join(ts_s_f, by='msno') %>%
  left_join(ul_s_c_f, by='msno') 
length( unique(joint1$msno) ) #to make sure joining correctly
fwrite(joint1, file = 'data/joint1.csv', append = FALSE, quote = "auto")

#split into train and test data
set.seed(321)
sample_msno <- sample(joint1$msno, size=0.2*nrow(joint1))
test<-joint1[joint1$msno %in% sample_msno, 2:ncol(joint1)]
'%!in%' <- function(x,y)!('%in%'(x,y)) #make an operator 
train<-joint1[joint1$msno %!in% sample_msno, 2:ncol(joint1)]
fwrite(test, file = 'data/test.csv', append = FALSE, quote = "auto")
fwrite(train, file = 'data/train.csv', append = FALSE, quote = "auto")



















