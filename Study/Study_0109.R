# Categorical variable = which has factor ex) Male, Female.
# Continuous variable = which has possibility to infinite ex) price.
# Logistic function is negative usually has good result.

# Read in dataset
quality = read.csv("quality.csv")
str(quality)

# Table outcome to see how many people received good care and how many poor care
# 0 = Good Care
# 1 = Poor Care
table(quality$PoorCare)

# First build a simple baseline model
# Most frequest outcome = baseline model for classification problem.
# Simple Baseline accuracy
98 / 131  #Most frequent / total


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
# False = Put the observation in thst set
split


# Create training and testing sets
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# Look at number of rows in training and test set
nrow(qualityTrain)
nrow(qualityTest)


# Logistic Regression Model with 2 independent variables
# Family = bionomial -> tells to build a logistic reg model
# GLM = Generalized Linear Model
QualityLog = glm(PoorCare ~ OfficeVisits +Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)


# Make predictions on training set
# Type response tells the predict function to give probabilities.
predictTrain = predict(QualityLog, type="response")


# Analyze predictions
# Since probabilitities, therefore, all number bet 0 and 1.
summary(predictTrain)

# Threshold value = it should be less to be good prediction model.

# if we're predicting higher probabilities for the actual poor care
# average prediction for each of the true outcome

tapply(predictTrain, qualityTrain$PoorCare, mean)
#     0           1
# 0.1894512       0.4392246
# all of the true poor care cases, we predict an average probabilities of about 0.44
# all of the true good care cases, we predict an average probabilities of about 0.19


# Confustion matrix for threshold of 0.5

table(qualityTrain$PoorCare, predictTrain > 0.5)


# Return TRUE if our prediction is greater than 0.5, which means 
# Return FALES if our prediction is less than 0.5, which means
# Sensitivity and specificity
10/(15+10)
70/(70+4)


# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain>0.7)
8 / 25
73 / 74


# Confusion matrix for threshold of 0.2
table(qualityTrain$PoorCare, predictTrain>0.2)
16 / 25
54 / 74


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
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2, 1.7))





# Homework_3
# No.4
quality = read.csv("quality.csv")
install.packages("caTools")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)
str(QualityLog)
table(QualityLog$PoorCare)



