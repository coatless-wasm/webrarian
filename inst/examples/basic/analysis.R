# Analysis

source("helpers.R")

data <- read.csv("data/sample.csv")
head(data)

plot(data$x, data$y, pch = 19, col = "steelblue", main = "Sample Data", xlab = "X", ylab = "Y")
abline(lm(y ~ x, data = data), col = "red", lwd = 2)
