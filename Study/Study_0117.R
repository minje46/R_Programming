# Random Forests [Another model]

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevenForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent
                            + LowerCourt + Unconst, data=Train, ntree=200,
                            nodesize=25)
# Warning : in CART, we added the argument method="class", 
# so that it was clear that we're doing a classification pro
# The randomForest function does not have a method argument.
# So when we want to do a classification problem,
# we need to make sure outcome is a factor.

#Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevenForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent
                            + LowerCourt + Unconst, data=Train, ntree=200,
                            nodesize=25)



# Make predictions
PredictForest = predict(StevenForest, newdata=Test)
table(Test$Reverse, PredictForest)

# Accuracy = 67%
# Accuracy logistic Reg = 66.5%
# Accuracy CART = 65.9%
(40+74) / (40+37+19+74)




# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


# Define cross-validation experiment
# number = # of folds 10 here
numFolds = trainControl(method="cv", number=10)


# Possible values for CP(Complexity parameter) parameter
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))


# Perform the cross validation
# method = 'rpart' -> cross validate a CART model
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
        data=Train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)

# get a table describing the cross validation accuracy for different cp parameters.
# First colunmn -> CP parameter that was tested
# Second column gives the cross validation accuracy for that
# The accuracy starts lower, and then increases, and then w
# Towards the end of the table it shows which  CP value to use

# Create a new CART model with the CP value suggested by ab
# method="class" -> building classification tree
library(rpart)
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

prp(StevensTreeCV)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
#Confus Matrix
# table(true outcome, prediction)
table(Test$Reverse, PredictCV)

#Accuracy = 0.724
#Accuracy of previous cart model =0.659
(59+64)/(59+18+29+64)


# By using Cross Validation we are selecting a 
PredictROC = predict(StevensTreeCV, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)
