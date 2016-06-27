# TINST490A Healthcare Informatics II
# Data Analysis and Visualization
# 7/31/2013 
# Si-Chi Chin

# Import data hepatitis.csv
# You would want to change the path of the file
data <- read.csv("~/Dropbox/UW-Tacoma/TINST490A_su13/Lecture_slides/Data_Visualization/hepatitis.csv")

# View the top of the data
head(data)

# Display the attributes of the data
names(data)

# Attach data
attach(data)

# Plot a histogram for age
hist(age)

# Plot a barplot for gender
barplot(table(gender))

# Select a subset of data 
female.age <- data[which(gender=='female'),2]
female.age

male.age <- data[which(gender=='male'),2]
male.age

length(male.age)
length(female.age)

# Plot a boxplot
?boxplot
boxplot(age~gender)

# Provide summary statistics
summary(age)

# Perform a t-Test
?t.test
t.test(age ~ gender)

# Perform a paired t-Test