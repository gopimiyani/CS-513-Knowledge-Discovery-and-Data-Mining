#################################################
#  Company    : Stevens 
#  Project    : CS-513
#  Purpose    : Final Exam Question - 3
#  First Name : Gopi
#  Last Name  : Miyani
#  Id			    : 10437266
#  Date       : 5/12/2019
#  Comments   : Following the instruction mentioned in the Final Exam Question - 3
#################################################
"
Use R/python to cluster (Algorithm=K-means; K=2) the seven (7) already normalized points in the accompanying table and answer a and b below:
"

rm(list = ls())

#Build Dataframe from the given Question Set
dataset <- data.frame("X" = c(1,5,4,4,1,4,2), "Y" = c(1,3,4,3,2,4,1), "Z" = c(1,4,5,4,1,4,2))
rownames(dataset) <- c("a", "b", "c", "d", "e", "f", "g")
dataset
#End Build Dataframe


#applying k means model and we set k==2
kmeans_2<- kmeans(dataset,2,nstart = 10)
# End Model Building

#Build Member of Cluster
kmeans_2$cluster
#End Cluster Building

#Coordinates with the cluster centers
kmeans_2$centers
#End

#Plot K-means cluster
plot(dataset, kmeans_2$cluster)
#End K-means Cluster Ploting