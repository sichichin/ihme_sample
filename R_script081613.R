# TINST490A Healthcare Informatics II
# Data Analysis and Visualization
# 8/16/2013 
# Si-Chi Chin

# Topic: Predict & 5-fold Cross Validation 

# Import data hepatitis.csv
# You would want to change the path of the file
hepatitis.data <- read.csv("~/Dropbox/UW-Tacoma/TINST490A_su13/Lecture_slides/Data_Visualization/hepatitis.csv", na.strings = "" )

attach(hepatitis.data)
View(hepatitis.data)

# Training and Testing
# Train on 2/3 of the data, test on the remaining 1/3
set.seed(1234) #Sample data
ind <- sample(2, nrow(hepatitis.data), replace=TRUE, prob=c(0.66, 0.34))
hepatitis.train <- hepatitis.data[ind==1,]
hepatitis.test <- hepatitis.data[ind==2,]
?predict

# Construct a logistics model using training data
hepatitis.glm <- glm(class ~  age +liverfirm + spiders + histology  , , family=binomial("logit"), data=hepatitis.train)
summary(hepatitis.glm)

# Test the model using testing data
test.pred <- predict(hepatitis.glm, newdata = hepatitis.test, type = "response")
head(test.pred)

added.attribute <- test.pred
added.attribute[test.pred > 0.5] <- "live"
added.attribute[test.pred <= 0.5] <- "die"



test.pred[test.pred > 0.5] <- "live"
test.pred[test.pred <= 0.5] <- "die"

table(test.pred, hepatitis.test$class)

matrix <- table(test.pred, hepatitis.test$class)
sum <- length(test.pred)
accuracy <- (matrix[1,1] + matrix[2,2]) / sum
accuracy