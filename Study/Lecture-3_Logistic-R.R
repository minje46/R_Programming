# Lecture 3, Logistic Regression

# V4

# Read in dataset
quality = read.csv("quality.csv")

# Look at structure
str(quality)

# Table outcome to see how many people received good care and how many poor care
# 0 = Good Care
# 1 = Poor Care
table(quality$PoorCare)

# First build a simple baseline model
# Most frequest outcome = baseline model for classification problem
# Simple Baseline accuracy
98/131

# Install and load caTools package
install.packages("caTools")
# Load the package
library(caTools)

# Randomly split data
set.seed(88) # So, that all of us get the same split
# 75% of data = Training Set to build the model
# 25% of data = Test Set to test the 
# Sample.split = 
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
# Look at the Split
# True = Put the observation in training set
# False = Put the observation in test set
split

# Create training and testing sets
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# Look at number of rows in training and test set
nrow(qualityTrain)
nrow(qualityTest)

# Logistic Regression Model with 2 independent variables
# Family = binomial-> tells to build a logistic reg model
# GLM = Generalized Linear Model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

# Make predictions on training set
# Type = response tells the predict function to give probabilities
predictTrain = predict(QualityLog, type="response")

# Analyze predictions
# SInce probabilities, therefore, all number bet 0 and 1
summary(predictTrain)

# if we're predicting higher probabilities for the actual poor care cases as we expect
# averge prediction for each of the true outcome

tapply(predictTrain, qualityTrain$PoorCare, mean)
#        0         1 
# 0.1894512 0.4392246 
# all of the true poor care cases, we predict an average probability of about 0.44
# all of the true good care cases, we predict an average probability of about 0.19

# V5

# Confusion matrix for threshold of 0.5
# first argument = what we want to label the rows by, should be the true outcome, which is qualityTrain$PoorCare
# second argument = what we want to label the columns by, will be predictTrain, or our predictions, greater than 0.5.

table(qualityTrain$PoorCare, predictTrain > 0.5)

# Return TRUE if our prediction is greater than 0.5, which means we want to predict poor care,
# Return FALSE if our prediction is less than 0.5, which means we want to predict good care

# Sensitivity and specificity
10/(15 + 10)
70/(70 + 4)

# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)

# Sensitivity and specificity
8/25
73/74

# Confusion matrix for threshold of 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)

# Sensitivity and specificity
16/25
54/74



# V6

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
