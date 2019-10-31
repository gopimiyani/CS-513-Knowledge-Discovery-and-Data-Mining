#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : HW 6_C5.0
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 4/9/2019
#  Comments   : Following the instruction mentioned in the Homework 6_6.2
#################################################

rm(list = ls())
file_name<-file.choose()

#Load Breast Cancer Dataset and factorizing Columns
BreastCancer_Data<-read.csv(file_name,na.strings = "?",
colClasses=c("Sample"="character",
             "F1"="factor","F2"="factor","F3"="factor",
             "F4"="factor","F5"="factor","F6"="factor",
             "F7"="factor","F8"="factor","F9"="factor",
             "Class"="factor"))
is.factor(BreastCancer_Data$F1)

#Eliminating Missing Values
BreastCancer_Data<-na.omit(BreastCancer_Data)

#Install packages
#install.packages("C50")
library('C50')
View(BreastCancer_Data)

summary(BreastCancer_Data)

#Split training and Testing
index<-sort(sample(nrow(BreastCancer_Data),round(.25*nrow(BreastCancer_Data))))
training<-BreastCancer_Data[-index,]
test<-BreastCancer_Data[index,]


?C5.0
C50_class<-C5.0(Class~.,data = training[,-1])
summary(C50_class)
plot(C50_class)   

C50_predicate<-predict(C50_class,test,type="class")
table(actual=test[,11],C50=C50_predicate)  

#Error Rate
wrong<-(test[,11]!=C50_predicate)
C50_rate<-sum(wrong)/length(test[,11])
C50_rate

#Accuracy
accuracy<-1-C50_rate
accuracy