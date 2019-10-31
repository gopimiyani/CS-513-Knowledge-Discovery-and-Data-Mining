#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : Midterm Q-4
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 3/31/2019
#  Comments   : Following the instruction mentioned in the Midterm Question-4
#################################################

'
#4 (10 Points)
•	Use R to create a vector of the following 20 numbers
•	Find maximum, minimum, median, mean and the standard deviation of the follow 20 numbers.
•	Replace the missing value with the mean of the numbers
•	Use R to develop a box plot for these numbers
45	48	6	42	49	63	81	56	21	75
25	48	56	24	73	82	NA	80	86	88
'

#Use R to create a vector of the following 20 numbers
my_vector<-c (45,	48,	6,	42,	49,	63,	81,	56,	21,	75, 25,	48,	56,	24,	73,	82,	NA,	80,	86,	88)

#Find maximum, minimum, median, mean and the standard deviation of the follow 20 numbers.
my_vector<-na.omit(my_vector)
min(my_vector,na.rm = TRUE)
max(my_vector,na.rm = TRUE)
median(my_vector,na.rm = TRUE)
mean(my_vector,na.rm = TRUE)
sd(my_vector,na.rm = TRUE)

#Replace the missing value with the mean of the numbers
my_vector_mean<-as.integer( ifelse(is.na(my_vector), mean(my_vector, na.rm=TRUE), my_vector))

#Use R to develop a box plot for these numbers
boxplot(my_vector,na.rm=TRUE)