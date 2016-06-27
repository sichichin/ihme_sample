# Diabetes Registry Data
# First created date: May 31, 2011
# http://en.wikipedia.org/wiki/Glycated_hemoglobin

# Read in data
data <- read.csv("~/Desktop/Diabetes Registry/dataset/DR_data.csv")
attach(data)

# Barplot view on Race
quartz()
barplot(table(data$RACE_NAME), las=1, horiz=TRUE, names = c("N/A", "Afr/Am", "Asian", "Caucsn", "Decl", "Hisp", "Multi", "Nat/Am", "Islander", "Other", "Unk"))
quartz()
barplot((table(data$RACE_NAME)/length(data$RACE_NAME)), las=1, horiz=TRUE, names = c("N/A", "Afr/Am", "Asian", "Caucsn", "Decl", "Hisp", "Multi", "Nat/Am", "Islander", "Other", "Unk"))

# Boxplots
boxplot(BMI_LAST~RACE_NAME,las=1, horizontal=TRUE, names = c("N/A", "Afr/Am", "Asian", "Caucsn", "Decl", "Hisp", "Multi", "Nat/Am", "Islander", "Other", "Unk"))

# Visualize basic data distribution
library(UsingR)
simple.hist.and.boxplot(BMI_LAST, main="Data distribution of BMI_LAST")


# Scatterplots
quartz()
pairs(data[,c(13,14)], col="red", cex=0.4, pch=16, main="HBA1C Scatterplot")

demo <- c(data[,c(13,14)],data[,c(2:4)])
pairs(demo, col="red", cex=0.4, pch=16, main="HBA1C + Demographics")

vital1 <- c(data[,c(13,14)],data[,c(5:8)])
pairs(vital1, col="red", cex=0.4, pch=16, main="HBA1C + BMI + WEIGH")

vital2 <- c(data[,c(13,14)],data[,c(9:12)])
pairs(vital2, col="red", cex=0.4, pch=16, main="HBA1C + BP_SYSTOLIC + BP_DIASTOLIC")

lab1 <- c(data[,c(13,14)],data[,c(15:18)])
pairs(lab1, col="red", cex=0.4, pch=16, main="HBA1C + \n MICROALBUMIN_POC_RNDM_MCG_MG_CR + \n BLOOD_UREA_NITROGEN")

lab2 <- c(data[,c(13,14)],data[,c(19:22)])
pairs(lab2, col="red", cex=0.4, pch=16, main="HBA1C + \n MICROALBUMIN_TIMED_COL + \n LDL_CHOLESTEROL_MEASURED")

lab3 <- c(data[,c(13,14)],data[,c(23:26)])
pairs(lab3, col="red", cex=0.4, pch=16, main="HBA1C + LDL_CHOLESTEROL_CALC + \n HDL_CHOLESTEROL_MEASURED_AND_CALC")

lab4 <- c(data[,c(13,14)],data[,c(27:28)])
pairs(lab4, col="red", cex=0.4, pch=16, main="HBA1C + NON_HDL_CHOLESTROL_CALC")

# Linear Regression
lm.out =lm(HBA1C_FIRST ~ PAT_AGE+BMI_FIRST+BMI_LAST+WEIGHT_FIRST+WEIGHT_LAST+BP_SYSTOLIC_FIRST+BP_SYSTOLIC_LAST+BP_DIASTOLIC_FIRST+BP_DIASTOLIC_LAST+MAL_POC_RNDM_MCG_MG_CR_FIRST+MAL_POC_RNDM_MCG_MG_CR_LAST+BLOOD_UREA_NITROGEN_FIRST+BLOOD_UREA_NITROGEN_LAST+MAL_TIMED_COL_FIRST+MAL_TIMED_COL_LAST+LDL_CHOL_MEAS_FIRST+LDL_CHOL_MEAS_LAST+LDL_CHOL_CALC_FIRST+LDL_CHOL_CALC_LAST+HDL_CHOL_MEAS_AND_CALC_FIRST+HDL_CHOL_MEAS_AND_CALC_LAST+NON_HDL_CHOL_CALC_FIRST+NON_HDL_CHOL_CALC_LAST)
summary(lm.out)

# this is wrong...
#summary(lm1 <- lm(HBA1C ~ ., data = data))

# Model selection
model.selection=step(lm.out)

step.out = lm(HBA1C_FIRST ~ PAT_AGE + BMI_FIRST + BMI_LAST + WEIGHT_FIRST + 
    WEIGHT_LAST + BP_SYSTOLIC_FIRST + BP_DIASTOLIC_FIRST + MAL_POC_RNDM_MCG_MG_CR_FIRST + 
    MAL_POC_RNDM_MCG_MG_CR_LAST + BLOOD_UREA_NITROGEN_LAST + 
    MAL_TIMED_COL_LAST + LDL_CHOL_MEAS_FIRST + LDL_CHOL_MEAS_LAST + 
    LDL_CHOL_CALC_FIRST + LDL_CHOL_CALC_LAST + HDL_CHOL_MEAS_AND_CALC_FIRST + 
    NON_HDL_CHOL_CALC_FIRST)
    
# Simple regression

mal.out =lm(HBA1C_FIRST~MAL_POC_RNDM_MCG_MG_CR_FIRST)
summary(mal.out)

pairs(HBA1C_FIRST~MAL_POC_RNDM_MCG_MG_CR_FIRST, col="red", cex=0.4, pch=16, main="HBA1C_FIRST + \n MAL_POC_RNDM_MCG_MG_CR_FIRST")

mal <- c(data[,c(13,14)],data[,c(15:16)])
pairs(mal, col="red", cex=0.4, pch=16, main="HBA1C + \n MAL_POC_RNDM_MCG_MG_CR")

