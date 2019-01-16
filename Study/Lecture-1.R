# Basic Calculations
8*6
2^16
2^
  8*6
8*10

# Functions
sqrt(2)
abs(-65)
?sqrt

# Variables
SquareRoot2 = sqrt(2)
SquareRoot2
HoursYear <- 365*24
HoursYear
ls()

# Vectors
c(2,3,5,8,13)
Country = c("Brazil", "China", "India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)
Country
LifeExpectancy
Country[1]
LifeExpectancy[3]
Sequence = seq(0,100,2)
Sequence

# Data Frames
CountryData = data.frame(Country, LifeExpectancy)
CountryData
CountryData$Population = c(199000,1390000,1240000,7997,318000)
CountryData
Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy, Population)
NewCountryData
AllCountryData = rbind(CountryData, NewCountryData)
AllCountryData

# Loading csv files
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)