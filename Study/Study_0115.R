# Lecture - CART

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)


# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)


# Install rapart library 
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)


# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket = 25)
# method = class -> rpart to build a classification tree instead of a regression tree
# minbucket: limits the tree so that it doesn't overfit to our training set (split을 나눌 숫자)
prp(StevensTree)


# Not sure about the abbrevations :
table(Train$Respondent)

# Make predictions
# type = "class" -> this argument required when making predictions
# for the CART model if want the majority class predictions
PredictCART = predict(StevensTree, newdata = Test, type="class")

# Confusion Matrix : Table(True outcome value, predictions)
table(Test$Reverse, PredictCART)


# Accuracy CART = 0.65
(41+71) / (41+36+22+71)
# Accuracy Logistic Reg = 0.665
# Accuracy Baseline model = 0.547


# ROC curve
library(ROCR)

# Compute predictions without the type="class"
PredictROC = predict(StevensTree, newdata = Test)
PredictROC
# Gives two number probability of outcome 0 and 1
# These numbers give the percentage of training set data in that subset with
# and the percentage of data in the training set in that subset with outcome

# Using second column to compute the ROC curve
pred = prediction(PredictROC[,2], Test$Reverse) # [,2] -> Using only second column
perf = performance(pred, "tpr", "fpr")
plot(perf)














