## ----results = "hide"----------------------------------------------------
library(RLab4)
library(mlbench)
library(caret)
data(BostonHousing)

trainIndex <- createDataPartition(BostonHousing$medv, p = .8, list = FALSE,times = 1)

Train <- BostonHousing[ trainIndex,]
Test  <- BostonHousing[-trainIndex,]

## ------------------------------------------------------------------------
 model1 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat , data=Train , method = 'lm')

## ------------------------------------------------------------------------
model2 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat , data=Train , method = 'leapForward')
summary(model2)

model2 <- train(medv ~ rm + dis + ptratio + lstat , data=Train , method = 'lm')

## ------------------------------------------------------------------------
summary(model1)
summary(model2)

## ------------------------------------------------------------------------
RidgeModel <- list(type=c("Regression"), library="RLab4",loop=NULL)
par <- data.frame(parameter="lambda", class="numeric",label="Lambda")
RidgeModel$parameters <- par

grid <- function(x, y, len = NULL, search = "grid"){
  if(search == "grid") {
    out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
  } else {
    out <- data.frame(lambda = 10^runif(len, min = -5, 1))
  }
  out
}
RidgeModel$grid <- grid

list_fit <- function(x, y, lambda, param, lev, last, classProbs, ...){
  data <- as.data.frame(x)
  data$response <- y
  
  formula <- "response ~ "
  cov <- capture.output(cat(colnames(data), sep="+"))
  formula <- paste(formula, cov)
  formula <- as.formula(formula)
  
  RLab4::ridgereg(formula=formula, data=data, lambda=param$lambda)
}
RidgeModel$fit <- list_fit

pred <- function(modelFit, newdata, preProc=NULL, submodels=NULL){
  # RLab4::modelFit$predict(newdata)
  # predict(modelFit, newdata)
}
RidgeModel$predict <- pred

prob<- list(NULL)
RidgeModel$prob <- prob

RidgeModel$label <- "Ridgeregression"

## ------------------------------------------------------------------------
model3 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat,data=Train,
                   method = "ridge")
model3

## ------------------------------------------------------------------------
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
model4 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat,data=Train,
                method = "ridge",
                trControl = fitControl)
model4

## ------------------------------------------------------------------------
model5 <- ridgereg(medv ~ crim + indus + zn + chas + 
           nox + rm + age + dis + rad + tax + ptratio 
         + b + lstat , data=Train , 0.0001)

## ------------------------------------------------------------------------
model5$summary()

## ---- fig.width = 7------------------------------------------------------
a <- predict(model1, newdata = Test)
plot(a,type="l",col="red")
lines(Test$medv, col="blue")

b <- predict(model2, newdata = Test)
plot(b,type="l",col="red")
lines(Test$medv, col="blue")

c <- model5$predict(data=Test)
plot(c,type="l",col="red")
lines(Test$medv, col="blue")

## ---- fig.width = 7------------------------------------------------------
rmse <- function(error){
  sqrt(mean(error^2))
}
error1 <- a-Test$medv
rmse(error1)
error2 <- b-Test$medv
rmse(error2)
error3 <- c-Test$medv
rmse(error3)

