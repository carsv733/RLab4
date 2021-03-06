## ------------------------------------------------------------------------
data(iris)
library(RLab4)
model <- linreg(Sepal.Length ~ Petal.Length,iris)

## ------------------------------------------------------------------------
model$plot()

## ----results = "hide"----------------------------------------------------
model$resid()

## ----results = "hide"----------------------------------------------------
model$pred()

## ------------------------------------------------------------------------
model$coef()

## ------------------------------------------------------------------------
model$summary()

