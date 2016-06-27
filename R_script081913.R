# TINST490A Healthcare Informatics II
# Data Analysis and Visualization
# 8/19/2013 
# Si-Chi Chin

# Topic: Clustering Analysis

# Import data hepatitis.csv
# You would want to change the path of the file
hepatitis.data <- read.csv("~/Dropbox/UW-Tacoma/TINST490A_su13/Lecture_slides/Data_Visualization/hepatitis.csv", na.strings = "" )



names(hepatitis.data)

hepatitis.data <- na.omit(hepatitis.data[, c(2,15:18)]) # Delete records with missing values and remove the class variable

head(hepatitis.data)
attach(hepatitis.data)


# K-Means Cluster Analysis, 5 clusters
kmeans.fit <- kmeans(hepatitis.data, 5)

# get cluster means 
aggregate(hepatitis.data,by=list(kmeans.fit$cluster),FUN=mean)
# append cluster assignment
hepatitis.data <- data.frame(hepatitis.data, kmeans.fit$cluster)
head(hepatitis.data)

# Hierarchical Clustering
d <- dist(hepatitis.data, method = "euclidean") # distance matrix
hc.fit <- hclust(d, method="single") 
plot(hc.fit) # display dendogram
groups <- cutree(hc.fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(hc.fit, k=5, border="red")

# Exercise: complete the code for clustering analysis using the data from Quiz 3 exercise
x <- c(0.40, 0.22, 0.35, 0.26, 0.08, 0.45)
y <- c(0.53, 0.38, 0.32, 0.19, 0.41, 0.30)
data <- cbind(x, y)
print(dist(data, method = "euclidean"),digit=2)
