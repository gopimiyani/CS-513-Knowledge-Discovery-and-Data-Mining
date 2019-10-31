#################################################
#  Company    : Stevens 
#  Project    : Cs-513
#  Purpose    : HW 3_Knn
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 3/15/2019
#  Comments   : Following the instruction mentioned in the Homework 3
#################################################

"
## EXERCISE ##
The “breast cancer dataset” in CANVAS was obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg.
The features in the dataset, described below, have been categorized from 1 to 10.
Use the knn methodology to develop a classification model for the Diagnosis. 
Important: make sure your categories are represented by the “factor” data type in R and delete the rows with missing value. 
Use 30% test 70% training data.      
Features                      Domain   -- -----------------------------------------   Sample code number              
id numberF1. Clump Thickness             
1 - 10   F2. Uniformity of Cell Size       
1 - 10   F3. Uniformity of Cell Shape      
1 - 10   F4. Marginal Adhesion             
1 - 10   F5. Single Epithelial Cell Size   
1 - 10   F6. Bare Nuclei                   
1 - 10   F7. Bland Chromatin               
1 - 10   F8. Normal Nucleoli              
1 - 10   F9. Mitoses            
1 - 10   Diagnosis Class:          
(2 for benign, 4 for malignant)

CORRECTION: ### IN CAPITAL LETTERS
"
# install kknn package
install.packages("kknn")
installed.packages()

#open kknn library to use it
library(kknn) 
?kknn

#Load BreastCancer dataset and attach it
file_name<-file.choose()
BreastCancer_rows<-read.csv(file_name,na.strings = "?") ## HERE, WE CAN DEFINE OTHER  THINGS
### bc_raw<-read.csv(f_name,na.strings="?", colClasses=c("SAmple"="character","F1"="factor",..till F9, "Class"="factor"))
                                  
View(BreastCancer_rows)
summary(BreastCancer_rows)
nrow(BreastCancer_rows)

#remove NA's
BreastCancer_rows<-na.omit(BreastCancer_rows)

# factorizing Class column
BreastCancer_rows$Class <- as.factor(BreastCancer_rows$Class)
View(BreastCancer_rows$Class)
is.factor(BreastCancer_rows$Class)
levels(BreastCancer_rows$Class) <- c("Benign", "Malignant")

#splitting datset into training and testing
idx<-sort(sample(nrow(BreastCancer_rows),as.integer(.70*nrow(BreastCancer_rows)))) 
training<-BreastCancer_rows[idx,]
test<-BreastCancer_rows[-idx,]
View(training)
View(test)


summary(training)
nrow(training)
nrow(test)
summary(test)

predicate_k7<-kknn(formula = Class~.,training,test,k=7,kernel = "rectangular")

### predicate_k7<-kknn(formula = Class~.,training[,-1],test,k=7,kernel = "rectangular") #subtracting 1st col as it is not required as in prediction--no mean

fit<-fitted(predicate_k7)
table(test$Class,fit)

""
### to got elbow curv, here k=3 as per PROF
### ------------------------    MIDTERM   --------------------------------
"
for (i in 1:10){

predicate<-kknn(formula = Class~.,training[,-1],test,k=i,kernel = 'rectangular') 
table(kknn=fit,test$Class)
knn_error_rate=sum(fit!test$Class)/length(test$Class)
print(knn_error_rate)
}
###
"
### knn_error_rate=sum(fit!test$Class)/length(test$Class)
### print(knn_error_rate)
'
predicate_k5<-kknn(formula = Class~.,training,test,k=5,kernel = "rectangular")
fit<-fitted(predicate_k5)#
table(test$Class,fit)

predicate_k2<-kknn(formula = Class~.,training,test,k=2,kernel = "rectangular")
fit<-fitted(predicate_k2)#
table(test$Class,fit)

predicate_k11<-kknn(formula = Class~.,training,test,k=11,kernel = "rectangular")
fit<-fitted(predicate_k11)#
table(test$Class,fit)

'

