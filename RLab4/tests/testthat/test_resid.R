data(iris)
model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)

test_that("Return vector of residuals", {
  expect_that(residuals(model2), equals(resid(model1)))
})