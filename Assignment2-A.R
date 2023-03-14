# Soomin Lee 040899389 CST8233 304 Assignment 2

# Part A - linear model on non-linear data
library(readxl)
library(zoom)
transistor <- read_excel("../Lab/TransistorCount.xlsx")

    # linear model
x.linear <- lm(`Transistor count`~`Release Year`, data=transistor)

    # plot 
par(mfrow=c(2,1))
plot(x.linear$model$`Release Year`, x.linear$model$`Transistor count`, 
     xlab="Year", ylab="Tansistor Count", 
     main="Soomin Lee", pch = 4)
    # add regression line
abline(x.linear, col = "red")

# Part B - log-linear model on non-linear data
    # non-linear model
x.loglinear <- lm(log(`Transistor count`)~`Release Year`, data = transistor)

    # plot
plot(transistor$`Release Year`, log(transistor$`Transistor count`),
     xlab = "Year", ylab = "Log Transistor count", main="Soomin Lee")

    # add regression line
abline(x.loglinear, col = "red")

# Part C - outliers
    # identify one distinct oulier 
outlier <- identify(transistor$`Release Year`, log(transistor$`Transistor count`))

    # add points
points(transistor$`Release year`[outlier], 
       log(transistor$`Transistor count`[outlier],
       col = "red", pch = 16))
