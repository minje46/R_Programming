4*5

#Loading csv files
WHO = read.csv("WHO.csv")
str(WHO)  # structure
summary(WHO)

#Subsetting
Who_Europe = subset(WHO, Region=="Europe")
Who_Europe
str(Who_Europe)
write.csv(Who_Europe, "WHO_Europe.csv")
getwd() #working directory.
rm(Who_Europe)
WHO$Under15

#Percentage of population Under 15
mean(WHO$Under15)

#Standard Deviation of percentage of population Under 15
sd(WHO$Under15)

summary(WHO$Under15)

# Conditions
which.min(WHO$Under15)
WHO$Country[86]

which.max(WHO$Under15)
WHO$Country[124]



#Scatter plot
plot(WHO$GNI, WHO$FertilityRate)

Outliners = subset(WHO, GNI>10000 & FertilityRate>2.5)
nrow(Outliners) # number of rows
Outliners[c("Country", "GNI", "FertilityRate")]


#Histograms
hist(WHO$CellularSubscribers)


#Boxplot
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expectancy Plot")



#Summary Tables
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LifeExpectancy, WHO$Region, min)
tapply(WHO$LifeExpectancy, WHO$Region, min, na.rm=TRUE) #na = Not Available
tapply(WHO$LifeExpectancy, WHO$Region, min, na.rm=FALSE) #na = Not Available


#Read in data
wine = read.csv("wine.csv")
str(wine)
summary(wine)

#Linear Regression(one variable)
model1 = lm(Price~AGST, data=wine)
summary(model1)


#Sum of squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Linear Regression (Two variable)
model2 = lm(Price~AGST +HarvestRain, data=wine)
summary(model2)

model3 = lm(Price ~ AGST +HarvestRain + WinterRain + Age +FrancePop, data=wine)
summary(model3)


model4 = lm(Price ~ AGST+ HarvestRain + WinterRain +Age, data=wine)
summary(model4)
