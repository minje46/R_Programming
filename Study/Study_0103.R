# Basic calculation
8*5
country = c("Korea", "USA", "China")
country
LifeExpectancy = c(74,23,51,67)
country[1]
LifeExpectancy[-1]
seq(0, 100, 3)
sequen e = s
Sequence = seq(0, 100, 5)
Sequence


#Data Frame
CountryData = data.frame(country, LifeExpectancy)
LifeExpectancy = c(74,23,51)
CountryData = data.frame(country, LifeExpectancy)
View(CountryData)
member = c("Kwak", "Kime", "Seung")
CountryData = data.frame(country, LifeExpectancy, member)
CountryData$LifeExpectancy
NewCountryData = data.frame("Swiss", "UK")

NewCountryData
AllCountryData = rbind(CountryData, NewCountryData)


#Loading csv files
WHO = read.csv("WHO.csv")
str(WHO)  # structure
summary(WHO)
setwd(directory~~)  #set directory

