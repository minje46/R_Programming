sweet = read.csv("data.csv")
summary(sweet)

# All variable
model = lm(High_Q.10kg. ~ avgTemp + maxTemp + minTemp + rainFall + wind_velocity + avgHumid + Cloud + Sunlight, data = sweet)
summary(model)


# Except avg Humid
model_h = lm(High_Q.10kg. ~ avgTemp + maxTemp + minTemp + rainFall + wind_velocity + Cloud + Sunlight, data = sweet)
summary(model_h)

# Using only one temperature variable
model_t = lm(High_Q.10kg. ~ avgTemp + rainFall + wind_velocity + Cloud + Sunlight, data = sweet)
summary(model_t)

# Calculate SSE
SSE = sum(model$residuals^2)
SSE

# Make test set predictions
sweetTest = read.csv("data_test.csv")
str(sweetTest)
predictTest = predict(model, newdata=sweetTest)
predictTest
View(sweetTest)
