#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : Midterm Q-5
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 3/31/2019
#  Comments   : Following the instruction mentioned in the Midterm Question-5
#################################################
'
The following two questions refer to the “IBM_attrition_v1.csv” dataset on canvas which is a subset of the “IBM attrition” dataset. 
The original dataset is used in IBM ML labs to uncover the factors that lead to employee attrition (attrition=yes).  
The dataset is a fictional data set created by IBM data scientists.

#5 (30 Points):  Classification using K Nearest Neighbor: 
Load IBM_attrition_v1.csv into R
a)	Remove any row with missing values
b)	Select every third record as the test dataset and the remaining records as the training dataset
c)	Preform K Nearest Neighbor ( K=3 unweighted)
d)	Score the test dataset
e)	Measure the error rate. 
'

rm(list=ls())

#Load IBM_attrition_v1.csv into R
file_name<-file.choose()
IBM_data<-read.csv(file_name)

View(IBM_data)
'
colClasses=c("Sample"="character",
"F1"="factor","F2"="factor","F3"="factor",
"F4"="factor","F5"="factor","F6"="factor",
"F7"="factor","F8"="factor","F9"="factor",
"Class"="factor"))
'
#a)	Remove any row with missing values
IBM_data<-na.omit(IBM_data)
str(IBM_data)
View(IBM_data)


#Minmax Normalization function
mnnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

# Normalize the data using Minmax Normalization function
IBM_data_normalized <- as.data.frame(
  cbind(Age= mnnorm(IBM_data[,1],min(IBM_data[,1]),max(IBM_data[,1])),
        JobSatisfaction= mnnorm(IBM_data[,2],min(IBM_data[,2]),max(IBM_data[,2])),
        MonthlyIncome= mnnorm(IBM_data[,3],min(IBM_data[,3]),max(IBM_data[,3])),
        YearsAtCompany= mnnorm(IBM_data[,4],min(IBM_data[,4]),max(IBM_data[,4])),
        Single = IBM_data[,5],
        Gender = IBM_data[,7],
        Attrition = IBM_data[,6]
  )
)

#b)	Select every third record as the test dataset and the remaining records as the training dataset
index<-seq(from=1,to=nrow(IBM_data_normalized),by=3)
test<-IBM_data_normalized[index,]
train<-IBM_data_normalized[-index,]

View(test)
View(train)

summary(test)
summary(train)

#c)	Preform K Nearest Neighbor ( K=3 unweighted)

library(kknn)
predict <- kknn(formula=factor(Attrition)~. ,  train,test  , kernel="rectangular", k=3)

#d)	Score the test dataset
fit <- fitted(predict)
table(kknn=fit,test$Attrition)

#e)	Measure the error rate. 
knn_error_rate=sum(fit!=test$Attrition)/length(test$Attrition)
print('Error Rate: ')
print(knn_error_rate)
knn_accuracy=1-knn_error_rate
print('Accuracy: ')
print(knn_accuracy)
'
for(i in 1:20) {
  predict <- kknn(formula=factor(Attrition)~. ,  train,test  , kernel="rectangular", k=i)
  
  #Extract fitted values from the object " "
  fit <- fitted(predict)
  table(kknn=fit,test$Attrition)
  knn_error_rate=sum(fit!=test$Attrition)/length(test$Attrition)
  print(i)
  print(knn_error_rate)
} 
'
