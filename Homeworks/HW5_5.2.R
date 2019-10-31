#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : HW 5_Cart
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 4/9/2019
#  Comments   : Following the instruction mentioned in the Homework 5_5.2
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
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")
#install.packages("RColorBrewer")

library(rpart)
library(rpart.plot)
library(rattlet)
library(RColorBrewer)

summary(BreastCancer_Data)

set.seed(111) #Random sampling, gives the intial seed for generationg random number
?ifelse

#Split training and Testing
index<-sort(sample(nrow(BreastCancer_Data),round(.25*nrow(BreastCancer_Data))))
training<-BreastCancer_Data[-index,]
test<-BreastCancer_Data[index,]

?rpart
dev.off()

CART_class <- rpart(Class~., data = training[,-1]) #Here, Class is the prediction column--target column
rpart.plot(CART_class)

CART_predict2 <- predict(CART_class, test, type = "class")
table(Actual = test[,11], CART= CART_predict2)

CART_predict <- predict(CART_class, test)
str(CART_predict)
CART_predict_cat<-ifelse(CART_predict[,1]<=.5,'Yes','No')
table(Actual=test[,11],CART=CART_predict_cat)

#Error Rate
CART_wrong<-sum(test[,11]!=CART_predict_cat)
CART_error_rate<-CART_wrong/length(test[,11])
CART_error_rate
CART_predict2<-predict(CART_class,test, type="class") #'class' here means we want result as classification model--a part of function
CART_wrong2<-sum(test[,11]!=CART_predict2)
CART_error_rate2<-CART_wrong2/length(test[,11])
CART_error_rate2

#Accuracy
accuracy=1-CART_error_rate2
accuracy
