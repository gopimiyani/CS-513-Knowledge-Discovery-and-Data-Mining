#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : Midterm Q-6
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 3/31/2019
#  Comments   : Following the instruction mentioned in the Midterm Question-6
#################################################

"
The following two questions refer to the “IBM_attrition_v1.csv” dataset on canvas which is a subset of the “IBM attrition” dataset. 
The original dataset is used in IBM ML labs to uncover the factors that lead to employee attrition (attrition=yes).  
The dataset is a fictional data set created by IBM data scientists.

6 (20 Points):  Naïve Bayes: 
Load IBM_attrition_v1.csv into R
a)	Remove any row with missing values
b)	Select every third record as the test dataset and the remaining records as the training dataset
c)	Preform Naïve Bayes using only  the following columns: “JobSatisfaction”, “Single” and   “Gender”  
d)	Score the test dataset
e)	Measure the error rate. 
"

rm(list=ls())

#Load IBM_attrition_v1.csv into R
file_name<-file.choose()
IBM_data<-read.csv(file_name)

View(IBM_data)

#a)	Remove any row with missing values
IBM_data<-na.omit(IBM_data)

View(IBM_data)

##Define max-min normalization function

mnnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

IBM_data_normalized <- as.data.frame(
  cbind(JobSatisfaction= mnnorm(IBM_data[,2],min(IBM_data[,2]),max(IBM_data[,2])),
        Single = IBM_data[,5],
        Gender = IBM_data[,7],
        Attrition = IBM_data[,6]
  )
)

is.factor(IBM_data_normalized$Attrition)
IBM_data_normalized$JobSatisfaction=as.factor(IBM_data_normalized$JobSatisfaction)
IBM_data_normalized$Single=as.factor(IBM_data_normalized$Single)
IBM_data_normalized$Gender=as.factor(IBM_data_normalized$Gender)
IBM_data_normalized$Attrition=as.factor(IBM_data_normalized$Attrition)

#b)	Select every third record as the test dataset and the remaining records as the training dataset
index<-seq(from=1,to=nrow(IBM_data_normalized),by=3)
test<-IBM_data_normalized[index,]
train<-IBM_data_normalized[-index,]

View(test)
View(train)

summary(test)
summary(train)
#c)	Preform Naïve Bayes using only  the following columns: “JobSatisfaction”,    “Single” and   “Gender”  
library(class) 
library(e1071)

nBayes <- naiveBayes(factor(Attrition)~JobSatisfaction+Single+Gender, data =train)

#d)	Score the test dataset
category_all<-predict(nBayes,test )
table(NBayes=category_all,Attrition=test$Attrition)

#e)	Measure the error rate. 
NB_wrong<-sum(category_all!=test$Attrition )
NB_error_rate<-NB_wrong/length(category_all)
print('Error Rate:')
print(NB_error_rate)
NB_accuracy=1-NB_error_rate
print('Accuracy:')
print(NB_accuracy)
