#Author: Devin Shryock
#City of Boston crime project to analyze crime in West Roxbury, MA
#This data is from https://data.cityofboston.gov
setwd("~/Desktop/LevelEDU /Labs/Week 4/Project#3")
crime.data <- read.csv("Crime_Incident_Reports.csv")

#Select data from crime in district E5, which is West Roxbury
west.roxbury.crime <- crime.data[which(crime.data$REPTDISTRICT == "E5"),]

#Remove crimes where the latitude is 0 - - this is missing data
west.roxbury.crime <- west.roxbury.crime[which(west.roxbury.crime$Lat != 0),]

#Plot crime latitudes using boxplot to identify outliers
boxplot(west.roxbury.crime$Lat,
        main ="Latitude of Crime in West Roxbury",
        col="green")
outliers.lat <- boxplot.stats(west.roxbury.crime$Lat)$out

#Plot crime longitudes using boxplot to indentify outliers
boxplot(west.roxbury.crime$Long,
        main = "Longitude of Crime in West Roxbury", 
        col="red")
outliers.long <- boxplot.stats(west.roxbury.crime$Long)$out

#Plot all longitude and latitude crime points
plot(west.roxbury.crime$Lat,west.roxbury.crime$Long,col="blue")

#Order the data by longitude from largest to smallest
#Remove the top 3 longitude points since they're outside West Roxbury
west.roxbury.crime <- west.roxbury.crime[order(-west.roxbury.crime$Long),]
west.roxbury.crime <- west.roxbury.crime[-c(1,2,3),]

#Plot all data points again since outliers are now gone
plot(west.roxbury.crime$Lat,west.roxbury.crime$Long,col="blue")

#############################################################################
#Look at data using k-means clustering

#Determine the number of clusters you should have (determine k)
k.values <- NULL
for (i in 2:10){
  k.values[i] <- kmeans(west.roxbury.crime[22:23], centers = i)$tot.withinss
}

#Plot k-values to determine where the elbow "breaks".  This is the point where
#the error stops decreasing as much.  This is the optimal number of kclusters
plot(k.values)

#Create 5 clusters of crime data points using kmeans clustering
cluster.west.roxbury.crime <- kmeans(west.roxbury.crime[22:23], centers = 5)
head(cluster.west.roxbury.crime)

#Plot the points in the each clusters by color
plot(west.roxbury.crime[22:23][,c("Long", "Lat")], 
     pch="o",
     col=cluster.west.roxbury.crime$cluster, 
     cex=.8,
     main="West Roxbury Crime by Cluster") 

#Returns the centers (Long, Lat) of each cluster 
cluster.centers <- cluster.west.roxbury.crime$centers

#Plot large yellow triangles at the center of each cluster
points(cluster.centers,pch=17,col="yellow",cex=3)


