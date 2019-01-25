# Lecture 4- CART 
# V4

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + 
Respondent + LowerCourt + Unconst, data = Train, 
method="class", minbucket=25)
#method = "class" --> rpart to build a classification tree 
#instead of a regression tree
# minbucket:  limits the tree so that it doesn't 
#overfit to our training set
prp(StevensTree)

#Not sure about the abbreviations:
table(Train$Respondent) 

# Make predictions
#type = "class" -> this argument required when making predictions 
# for the CART model if  want the majority class predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")

#Confusion Matrix:
# Table(True outcome value, predictions)
table(Test$Reverse, PredictCART)

# Accuracy CART = 0.65
(41+71)/(41+36+22+71)
#Accuracy Logistic Reg = 0.665
# Accuracy Baseline model = 0.547

# ROC curve
library(ROCR)

#Compute predictions without the type = 'Class'
PredictROC = predict(StevensTree, newdata = Test)
PredictROC #Gives two number probability of outcome 0 and 1
#These numbers give the percentage of training set data in that subset with outcome 0
# and the percentage of data in the training set in that subset with outcome 1.

# Using second column to compute the ROC curve
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# 5 - Random Forests

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
# warning: in CART, we added the argument method="class",
# so that it was clear that we're doing a classification problem.
# The randomForest function does not have a method argument.
# So when we want to do a classification problem,
# we need to make sure outcome is a factor.

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)

#Accuracy = 67%
#Accuracy Logistic Reg = 66.5%
# Accuracy CART = 65.9%
(40+74)/(40+37+19+74)


# V6

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
# number = # of folds 10 here
numFolds = trainControl( method = "cv", number = 10 )

# Possible values for CP (Complexity parameter) parameter 
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
# method = 'rpart' -> cross validate a CART model 
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# get a table describing the cross validation accuracy for different cp parameters.
# First column -> CP parameter that was tested 
# second column gives the cross validation accuracy for that cp value.
# The accuracy starts lower, and then increases, and then will start decreasing again (shown in slides) 
# Towards the end of the table it shows which CP value to use

# Create a new CART model with the CP value suggested by above model
# method = 'class' -> building classification tree
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")

# Confusion Matrix
# table (true outcome, prediction)
table(Test$Reverse, PredictCV)

# Accuracy = 0.724
# Accuracy of previous cart model = 0.659
(59+64)/(59+18+29+64)

# By using Cross Validation, we are selecting a SMART parameter value. 

