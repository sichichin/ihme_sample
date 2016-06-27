# TINST490A Healthcare Informatics II
# Data Analysis and Visualization
# 8/14/2013 
# Si-Chi Chin

# Topic: Decision Tree & K Nearest-Neighbor

# Import data hepatitis.csv
# You would want to change the path of the file
data <- read.csv("~/Dropbox/UW-Tacoma/TINST490A_su13/Lecture_slides/Data_Visualization/hepatitis.csv", na.strings = "" )

attach(data)

# Construct a decision tree using "rpart" package
library(rpart)
?rpart

# Use all variables to predict the class label
formula <- class ~ .
fit.tree <- rpart(formula, data=data)
fit.tree

# Plot the decision tree
plot(fit.tree, uniform=TRUE, cex=0.8, main="Decision Tree Using rpart", compress =TRUE)
text(fit.tree, cex=0.8,  use.n=TRUE, all=TRUE)

# Construct a decision tree using "party" package
library(party)
formula1 <- class ~  .
fit1.tree <- ctree(formula1, data=data)
print(fit1.tree)
plot(fit1.tree, cex=0.5)
plot(fit1.tree, type="simple")


# Training and Testing
# Train on 2/3 of the data, test on the remaining 1/3
set.seed(1234) #Sample data
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.66, 0.34))
hepatitis.train <- data[ind==1,]
hepatitis.test <- data[ind==2,]


# Build a decision tree on training data
hepatitis.tree <- rpart(formula, method = "class",data=hepatitis.train, control = rpart.control(minsplit = 5))
hepatitis.tree <- rpart(formula, method = "class",data=hepatitis.train)
print(hepatitis.tree)
summary(hepatitis.tree)
plot(hepatitis.tree, uniform=TRUE, cex=0.8, main="Classification for Hepatitis Data", compress=TRUE)
text(hepatitis.tree, use.n=TRUE, all=TRUE, cex=0.8)

fit2.tree <- ctree(formula1, data=hepatitis.train)
print(fit2.tree)
plot(fit2.tree)
plot(fit2.tree, type = "simple")


# Make prediction using testing data
hepatitis.pred <- predict(hepatitis.tree, newdata=hepatitis.test, type="class")
hepatitis.pred

# View confusion matrix for the results from rpart
table(hepatitis.pred, hepatitis.test$class)

fit2.pred <- predict(fit2.tree, newdata=hepatitis.test)

# View confusion matrix for the results from party
table(fit2.pred, hepatitis.test$class)

# Compute accuracy
matrix <- table(fit2.pred, hepatitis.test$class)
sum <- length(fit2.pred)
accuracy <- (matrix[1,1] + matrix[2,2]) / sum
accuracy

# K Nearest Neighbor Classification

# Remove rows with missing values
# Select *only* the class column and numeric attributes
data.complete <- na.omit(data[,c(1,2,15,16,17,18)])

set.seed(1234) #Sample data
ind <- sample(2, nrow(data.complete), replace=TRUE, prob=c(0.66, 0.34))
complete.train <- data.complete[ind==1,]
complete.test <- data.complete[ind==2,]

length(complete.train$class)
length(complete.test$class)

# Train on 2/3, test on 1/3
hepatitis.knn <- knn(complete.train[,-1], complete.test[,-1], complete.train[,1], k = 5, prob=TRUE)

# View the confusion matrix
table(hepatitis.knn, complete.test$class)
matrix <- table(hepatitis.knn, complete.test$class)
sum <- length(hepatitis.knn)
accuracy <- (matrix[1,1] + matrix[2,2]) / sum
accuracy

# Leave-one-out cross validation
cv.knn <-knn.cv(complete.train[,-1],  complete.train[,1], k = 5, prob=TRUE)

# View the confusion matrix
table(cv.knn, complete.train$class)

# What is the accuracy for the cross validation results?

