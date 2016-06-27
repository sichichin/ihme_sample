# TINST490A Healthcare Informatics II
# Data Analysis and Visualization
# 8/2/2013 
# Si-Chi Chin

# Import data hepatitis.csv
# You would want to change the path of the file
data <- read.csv("~/Dropbox/UW-Tacoma/TINST490A_su13/Lecture_slides/Data_Visualization/hepatitis.csv", na.strings = "" )

# Draw a scatterplot with variables related to tests for liver function
pairs(data[15:19], cex=0.5, col="red")

# Fit a simple regression between "alk" (y) and "sgot" (x)
?lm
fit.lm <- lm(alk ~ sgot) #You may use any other name to replace fit.lm

# Provide a summary of the fitted regression line
summary(fit.lm)


# Handle missing value
# Note: the value zero is not the same as missing values
?lm


# What is the p-value for the slope? How to interpret the results?


# RSS interpretation: X-variable "explains" ___% of the variation in the Y-variable.


# What is the squreroot of RSS? How does it compare to the results of correlation analysis?


# Try another dataset iris
data(iris)
plot(iris)
summary(lm(Petal.Length~Petal.Width, data=iris))


# Correlation analysis
?cor
cor(sgot,alk)

# Handle missing value
cor(sgot, alk, use="complete.obs")

# Correlation test
?cor.test
cor.test(sgot, alk)

# Logistic Regression
?glm
class.glm <- glm(class ~ age, family=binomial("logit"))
summary(class.glm)

categorical.glm <- glm(class ~ fatigue, family=binomial("logit"))
summary(categorical.glm)


# What are the other variables that can predict the "class" attribute (live or die)?