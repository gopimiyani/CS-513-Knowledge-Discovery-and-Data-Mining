#################################################
#  Company    : Stevens 
#  Project    : Cs-513
#  Purpose    : HW 2_EDA
#  First Name  : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 3/15/2019
#  Comments   : Following the instruction mentioned in the Homework 2
#################################################

## EXERCISE ##
"1-Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the EDA analysis by:
I.Summarizing each column (e.g. min, max, mean )
II.Identifying missing values
III.Replacing the missing values with the “mean” of the column.
IV.Displaying the frequency table of “Class” vs. F6
V.Displaying the scatter plot of F1 to F6, one pair at a time
VI.Show histogram box plot for columns F7 to F9

2- Delete all the objects from your R- environment. 
Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
Remove any row with a missing value in any of the columns."


#******************************************************************
rm(list=ls())

#1-Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the EDA analysis by:
file_name<-file.choose()
BreastCancer_rows<-read.csv(file_name,na.strings = "?")
View(BreastCancer_rows)

#I.Summarizing each column (e.g. min, max, mean )
summary(BreastCancer_rows)


#II.Identifying missing values

missing_values<-is.na(BreastCancer_rows)
total_missing_values<-sum(is.na(BreastCancer_rows))

### SOLN- missing<-bc[is.na(bc$F6)] #here, bc=data
  
### MODE functon ----not in midterm
modes<- function(x){
  unique.x<-unique(x)
  tab<-tabulate(match(x,unique.x))
  unique.x[tab==max(tab)]
}

#III.Replacing the missing values with the “mean” of the column.

for(i in 1:ncol(BreastCancer_rows)){
  BreastCancer_rows[is.na(BreastCancer_rows[,i]), i] <- as.integer( mean(BreastCancer_rows[,i], na.rm = TRUE))
}

View(BreastCancer_rows)
summary(BreastCancer_rows)

#IV.Displaying the frequency table of “Class” vs. F6

class(BreastCancer_rows)
table(class=BreastCancer_rows$Class,F6=BreastCancer_rows$F6)


#V.Displaying the scatter plot of F1 to F6, one pair at a time
pairs(BreastCancer_rows[2:7])

#VI.Show histogram box plot for columns F7 to F9
?hist
boxplot(BreastCancer_rows[8:10])
#boxplot(BreastCancer_rows[8])
hist(BreastCancer_rows$F7)
#boxplot(BreastCancer_rows[9])
hist(BreastCancer_rows$F8)
#boxplot(BreastCancer_rows[10])
hist(BreastCancer_rows$F9)


#boxplot(BreastCancer_rows, horizontal=TRUE,  outline=TRUE,ylim=c(-2,4), frame=F, col = "green1")
#hist(BreastCancer_rows,xlim=c(-4,4), col = "pink")

#2- Delete all the objects from your R- environment. 
rm(list=ls())

#Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
file_name<-file.choose()
BreastCancer_rows<-read.csv(file_name,na.strings = "?")

#Remove any row with a missing value in any of the columns."
BreastCancer_rows<-na.omit(BreastCancer_rows)

summary(BreastCancer_rows)

