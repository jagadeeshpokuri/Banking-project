library(dplyr)
library(tidyr)
library(visdat)
library(dplyr)

setwd('D:\\Edvancer\\Projects\\Banking')

bank_train=read.csv('bank-full_train.csv', stringsAsFactors = FALSE)
bank_test=read.csv('bank-full_test.csv', stringsAsFactors= FALSE)

bank_test$y= NA

bank_train$data='train'
bank_test$data='test'

bank_main=rbind(bank_train, bank_test)

vis_dat(bank_main)


glimpse(bank_main)

table(bank_main$data)

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
bank_main=CreateDummies(bank_main,'job')
bank_main=CreateDummies(bank_main, 'marital')
bank_main=CreateDummies(bank_main,'education')
bank_main=CreateDummies(bank_main, 'default')
bank_main=CreateDummies(bank_main,'housing')
bank_main=CreateDummies(bank_main,'loan')
bank_main=CreateDummies(bank_main,'contact')
bank_main=CreateDummies(bank_main,'month')
bank_main=CreateDummies(bank_main,'poutcome')

vis_dat(bank_main, warn_large_data= FALSE)
glimpse(bank_main)
library(dplyr)
bank_train=bank_main %>% filter(data=='train') %>% select(-data)
bank_test=bank_main %>% filter(data=='test')%>% select(-data,-y)


set.seed(2)
s=sample(1:nrow(bank_train),0.8*nrow(bank_train))
train1=bank_train[s,]
train2=bank_train[-s,]

library(car)



Quiz questions

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
