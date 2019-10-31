#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : HW 07
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 5/3/2019
#  Comments   : Following the instruction mentioned in the HW 07
#################################################
"
Apply ANN in R or Python to the “IBM Employee Attrition V2” dataset in CANVAS to uncover the features that can predict employee attrition. 
This is a subset of a fictional data set created by IBM data scientists. 
Use every 5th record to create the test dataset and use the remaining records to create the training dataset. 
Do not normalize the data. 
Hint:
(Use the “compute” function to score the test dataset  
e.g  net.score <- compute(your_ann, testdata))
"

rm(list=ls())

#Reading Data
file_name <- file.choose()
IBM_data<- read.csv(file_name)
View(IBM_data)

#remove all the records with missing value
IBM_data2 <- data.frame(lapply(na.omit(IBM_data), as.numeric))

#Split training and testing
index <- seq (1,nrow(IBM_data2),by=5)
test<- IBM_data2[index,]
training<-IBM_data2[-index,]

#Apply Neural Network
library("neuralnet")
?neuralnet()
class(training$Attrition)
net_IBM_data2<- neuralnet( Attrition~. ,training, hidden=10, threshold=0.01)


#Plot the neural network
plot(net_IBM_data2)

# test should have only the input colum
ann <-compute(net_IBM_data2 , test[,-2])
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)
table(Actual=test$Attrition,predition=ann_cat)

#Error Rate
wrong<- (test$Attrition!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate

#Accuracy
accuracy<-1-error_rate
accuracy