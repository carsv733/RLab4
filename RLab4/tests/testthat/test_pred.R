data(iris)
model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)

test_that("Return vector of predicted values yhat", {
  expect_that(fitted.values(model2), equals(pred(model1)))
})