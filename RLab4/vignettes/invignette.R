library(RLab4)
library(mlbench)
library(caret)
data(BostonHousing)

trainIndex <- createDataPartition(BostonHousing$medv, p = .8, list = FALSE,times = 1)

head(trainIndex)
Train <- BostonHousing[ trainIndex,]
Test  <- BostonHousing[-trainIndex,]

#Fit a linear regression model with forward selection of covariates 
# on the training dataset. 

model1 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat,data=Train,method = 'lm')
model2 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat,data=Train,method = 'leapForward')
model2 <- train(medv ~ rm +  ptratio + lstat, data=Train,method = 'lm')

summary(model1)
summary(model2)



modelInfo <- list(label = "Ridge Regression",
                  library = "RLab4",
                  type = "Regression",
                  parameters = data.frame(parameter = c('lambda'),
                                          class = c("numeric"),
                                          label = c('Weight Decay')),
                  grid = function(x, y, len = NULL, search = "grid")  {
                    if(search == "grid") {
                      out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    enet(as.matrix(x), y, lambda = param$lambda)  
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata, 
                            s = 1, 
                            mode = "fraction")$fit
                  },
                  predictors = function(x, s = NULL, ...) {
                    if(is.null(s))
                    {
                      if(!is.null(x$tuneValue))
                      {
                        s <- x$tuneValue$.fraction
                      } else stop("must supply a vaue of s")
                      out <- predict(x, s = s,
                                     type = "coefficients",
                                     mode = "fraction")$coefficients
                      
                    } else {
                      out <- predict(x, s = s)$coefficients
                      
                    }
                    names(out)[out != 0]
                  },
                  tags = c("Linear Regression", "L2 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(-x$lambda),])




model3 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat,data=Train,
                   method = modelInfo)


fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
model4 <- train(medv ~ crim + indus + zn + chas + 
                  nox + rm + age + dis + rad + tax + ptratio 
                + b + lstat,data=Train,
                method = modelInfo,
                trControl = fitControl)

model5 <- ridgereg(medv ~ crim + indus + zn + chas + 
           nox + rm + age + dis + rad + tax + ptratio 
         + b + lstat,data=Train,0.0001)

model5$summary()


a <- predict(model1, newdata = Test)
plot(a,type="l",col="red")
lines(Test$medv, col="blue")

b <- predict(model2, newdata = Test)
plot(b,type="l",col="red")
lines(Test$medv, col="blue")

med <- NULL
for (j in 1:length(Test$zn)){
  med[j] <- (model5$coef()[1] + model5$coef()[2]*Test$crim[j] + model5$coef()[3]*Test$indus[j]
    + model5$coef()[4]*Test$zn[j] + model5$coef()[6]*Test$nox[j] + model5$coef()[7]*Test$rm[j] 
    + model5$coef()[8]*Test$age[j] + model5$coef()[9]*Test$dis[j] 
    + model5$coef()[10]*Test$rad[j] + model5$coef()[11]*Test$tax[j] 
    + model5$coef()[12]*Test$ptratio[j] + model5$coef()[13]*Test$b[j]
    + model5$coef()[14]*Test$lstat[j])
  med <- append(med,med[j])
}


chas

med <- med[-100]
error <- med-Test$medv
# Function that returns Root Mean Squared Error
rmse <- function(error){
  sqrt(mean(error^2))
}
