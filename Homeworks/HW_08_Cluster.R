#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : HW-8
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 5/3/2019
#  Comments   : Following the instruction mentioned in the HW 08
#################################################
"
Cluster-1
>> Using hclust, categorize the “IBM_employee_attrition_v2.csv” data into two (2) clusters based on 
“TotalWorkingYears”, “Gender” and “Education” features. 
>> Tabulate the clustered rows against the “Attrition” column. (Remove the rows with missing values first)

Cluster-2
Using k-means, categorize the “IBM_employee_attrition_v2.csv” data into two (2) clusters based on 
“TotalWorkingYears”, “Gender” and “Education” features. Tabulate the clustered rows against the “Attrition” column. 
(Remove the rows with missing values first)
"

rm(list=ls())

#Load IBM_attrition_v1.csv into R
file_name<-file.choose()
IBM_data<-read.csv(file_name)

View(IBM_data)
summary(IBM_data)

#Remove any row with missing values

IBM_data2 <- data.frame(lapply(na.omit(IBM_data), as.numeric))

##Cluster-1 >> Using hclust, categorizing IBM data into two (2) clusters based on “TotalWorkingYears”, “Gender” and “Education” feature
IBM_data_dist<-dist(IBM_data2[,c("TotalWorkingYears", "Gender", "Education")])
hclust_resutls<-hclust(IBM_data_dist)
plot(hclust_resutls)
hclust_2<-cutree(hclust_resutls,2)

#Tabulate the clustered rows against the “Attrition” column.
table(hclust_2,IBM_data2[,2])

#Error Rate
wrong<- (IBM_data2$Attrition!=hclust_2)
error_rate<-sum(wrong)/length(wrong)
error_rate

#Accuracy
accuracy<-1-error_rate
accuracy

##Cluster-2 >> Same clusters as Cluster1 but using K-means 

?kmeans
kmeans_2<- kmeans(IBM_data2[,c("TotalWorkingYears", "Gender", "Education")],2,nstart = 10)
str(kmeans_2)
kmeans_2$cluster
table(kmeans_2$cluster,IBM_data2[,2])

#Error Rate
wrong<- (IBM_data2$Attrition!=kmeans_2$cluster)
error_rate<-sum(wrong)/length(wrong)
error_rate

#Accuracy
accuracy<-1-error_rate
accuracy
