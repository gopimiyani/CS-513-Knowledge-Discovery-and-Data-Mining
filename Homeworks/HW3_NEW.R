install.packages("kknn") # kknn coz it can handle both weighted and un-weighted knn.
installed.packages()

library(kknn) #open that library to use their functions
?kknn
#Load the dataset and attach it

file_name<-file.choose()
BreastCancer_rows<-read.csv(file_name,na.strings = "?")
View(BreastCancer_rows)


BreastCancer_rows<-na.omit(BreastCancer_rows)
idx<-sort(sample(nrow(BreastCancer_rows),as.integer(.70*nrow(iris)))) #(.65*nrow(iris) =97.5 as we can't take half sample, need to convert integer and the o/p will be the samples of 97 from 150
training<-iris[idx,]
test<-iris[-idx,]

