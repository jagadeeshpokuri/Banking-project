library(dplyr)
library(stringr)
library(data.table)
library(MASS)
library(randomForest)
library(purrr)
library(caret)
library(visdat)
library(tidyr)


setwd('D:\\Edvancer\\Projects\\Banking')

b_train=read.csv('bank-full_train.csv', stringsAsFactors = FALSE)
b_test=read.csv('bank-full_test.csv', stringsAsFactors= FALSE)

b_test$y= NA

b_train$data='train'
b_test$data='test'

b_main=rbind(b_train, b_test)

vis_dat(b_main)


#Convert character to dummies 
#job, marital, education, default, housing, loan, contact, month, poutcome

glimpse(b_main)

table(b_main$data)

CreateDummies=function(data,var,freq_cutoff=0                                                                                                      ){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
vis_dat(b_main, warn_large_data= FALSE)
b_main=CreateDummies(b_main,'job')
b_main=CreateDummies(b_main, 'marital')
b_main=CreateDummies(b_main,'education')
b_main=CreateDummies(b_main, 'default')
b_main=CreateDummies(b_main,'housing')
b_main=CreateDummies(b_main,'loan')
b_main=CreateDummies(b_main,'contact')
b_main=CreateDummies(b_main,'month')
b_main=CreateDummies(b_main,'poutcome')

vis_dat(b_main, warn_large_data= FALSE)
glimpse(b_main)

library(dplyr)
b_train=b_main %>% filter(data=='train') %>% select(-data)
b_test=b_main %>% filter(data=='test')%>% select(-data,-y)



library(h2o)
h2o.init(nthreads= 3)

train= as.h2o(b_train)
test= as.h2o(b_test)

y= 'y'

x=setdiff(names(train), y)

train[,y]= as.factor(train[,y])

aml= h2o.automl(x= x,
                y= y, 
                training_frame = train, 
                max_models = 10)

b_best <- aml@leaderboard
print(b_best,n = nrow(b_best))

#bm <- h2o.get_best_model(aml)
b_model <- h2o.getModel("GLM_1_AutoML_1_20230416_143356")
b_model
x=as.data.frame(h2o.predict(b_model,test))

colnames(x)[1]="y"
x=x %>% select(-no,-yes)

write.csv(x,"D:\\Edvancer\\Projects\\Banking\\submission.csv",row.names = F)

####### calculating score from the result of automl model #######
TP=2705
FN=1015
TN=25412
FP=2512

P=TP+FN
N=TN+FP

Sn=TP/P
Sp=TN/N
precision=TP/(TP+FP)
recall=Sn

KS=(TP/P)-(FP/N)
F5=(26*precision*recall)/((25*precision)+recall)
F.1=(1.01*precision*recall)/((.01*precision)+recall)

M=(4*FP+FN)/(5*(P+N))

Sn
Sp
KS
F5
F.1
M

###############  Quiz questions

setwd('D:\\Edvancer\\Projects\\Banking')

bank_train=read.csv('bank-full_train.csv', stringsAsFactors = FALSE)

vis_dat(bank_train, warn_large_data= FALSE)

bank_train=CreateDummies(bank_train,'job')
bank_train=CreateDummies(bank_train, 'marital')
bank_train=CreateDummies(bank_train,'education')
bank_train=CreateDummies(bank_train, 'default')
bank_train=CreateDummies(bank_train,'housing')
bank_train=CreateDummies(bank_train,'loan')
bank_train=CreateDummies(bank_train,'contact')
bank_train=CreateDummies(bank_train,'month')
bank_train=CreateDummies(bank_train,'poutcome')

vis_dat(bank_train, warn_large_data= FALSE)
glimpse(bank_train)

age_avg=round(mean(bank_train$age),2)
age_avg

bal_var=var(bank_train$balance)
bal_var
