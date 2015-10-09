library(RLab4)
library(mlbench)
library(caret)

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

summary(model1)
summary(model2)


model_info <- getModelInfo(model = "ridge", regex = FALSE)[[1]]
## Model components
names(model_info)



