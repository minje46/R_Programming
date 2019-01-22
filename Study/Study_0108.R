# HW1_Question 4. 
model = lm (Price~ HarvestRain + WinterRain, data=wine)
summary(model)

# HW1_Question 5.
cor(wine$HarvestRain, wine$WinterRain)


# Read in data.
wine = read.scv("wine.csv")
str(wine)


# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)  # Price is gonna be Y. AGST is gonna be x.
summary(model1)   #B0 = intercept value which is found already.

# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE


# Linear Regression (Two variables)
model2 = lm(Price ~ AGST +HarvestRain, data=wine)
summary(model2)   # *** means how significant vlaue is. If the variable is not significant, it can be skipped.
SSE = sum(model2$residuals^2) # residuals is automatic calculation of each sse.

model3 =lm(Price ~ ., data=wine) # . means all of columns.
summary(model3)

model4 = lm(Price ~ AGST +HarvestRain +WinterRain +Age, data=wine)
summary(model4)
SSE = sum(model4$residuals^2)
SSE

model5 = lm(Price ~ AGST +HarvestRain +WinterRain +FrancePop, data=wine)
summary(model5)
SSE = sum(model5$residuals^2)
SSE


# Correlations
cor(wine$WinterRain, wine$Price)  # High positive(close to 1) is good way.
cor(wine$Age, wine$FrancePop) # Negative is not good way.
cor(wine)   # absolute calculation is close to 1 is good.


# Make test set predictions
wineTest = read.csv("Wine_test.csv")
str(wineTest)
predictTest = predict(model4, newdata=wineTest)
predictTest
View(wineTest)

# Compute R-squared
SSE = sum((wineTest$Price - predictTest)^2)
SSE
SST = sum((wineTest$Price - mean(wine$Price))^2)
SST
1-SSE/SST   #R^2      Closing to 1 is the best way of prediction.

