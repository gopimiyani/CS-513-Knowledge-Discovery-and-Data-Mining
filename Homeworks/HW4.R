#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : HW 4_Naive Bayes
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 3/21/2019
#  Comments   : Following the instruction mentioned in the Homework 4
#################################################

"The “breast cancer dataset” in CANVAS was obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg. 
The features in the dataset, described below, have been categorized from 1 to 10.
Use the Naïve Bayes methodology to develop a classification model for the Diagnosis.
Important: make sure your categories are represented by the “factor” data type in R and delete the rows with missing values. 
Use 30% test 70% training data       
Features                      Domain   -- -----------------------------------------   Sample code number               id number   
F1. Clump Thickness               1 - 10   
F2. Uniformity of Cell Size       1 - 10  
F3. Uniformity of Cell Shape      1 - 10   
F4. Marginal Adhesion             1 - 10  
F5. Single Epithelial Cell Size   1 - 10   
F6. Bare Nuclei                   1 - 10   
F7. Bland Chromatin               1 - 10   
F8. Normal Nucleoli               1 - 10   
F9. Mitoses                       1 - 10   
Diagnosis Class:                 (2 for benign, 4 for malignant)"

rm(list=ls())

#Load BreastCancer dataset and attach it
file_name<-file.choose()
BreastCancer_rows<-read.csv(file_name,na.strings = "?")

# Delete the rows with missing values
BreastCancer_rows<-na.omit(BreastCancer_rows)

# factorizing Class column

BreastCancer_rows$Class <- as.factor(BreastCancer_rows$Class)
levels(BreastCancer_rows$Class) <- c("Benign", "Malignant")


# Install packages e1071 needed for naive bayes
#install.packages("e1071",dependencies = TRUE)
library(class)
library(e1071)


#splitting datset into training and testing
idx<-sort(sample(nrow(BreastCancer_rows),as.integer(.70*nrow(BreastCancer_rows)))) 
training<-BreastCancer_rows[idx,]
test<-BreastCancer_rows[-idx,]
#View(training)
#View(test)


##NAive Byes
?naiveBayes()

##Compare the predication to actual
nBayes_all<-naiveBayes(Class ~., data=BreastCancer_rows)

category_all<-predict(nBayes_all,BreastCancer_rows)
table(nBayes_all=category_all,Class=BreastCancer_rows$Class) 

##NO of mis classification
NB_wrong<-sum(category_all!=BreastCancer_rows$Class) 
NB_error_rate<-NB_wrong/length(category_all)

# Accuracy
accuracy = 1 - NB_error_rate
accuracy

### NAive BAyes is a very good in real manner. then only think other than that. works well when not many predictors. if it is very large, then performance may degrade and others work well

###--------------- MIDTERM -------- to pick every 5th column or 3rd column...
