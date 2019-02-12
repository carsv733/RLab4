test_that("Return vector of residuals", {
  data(iris)
  model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)
  expect_equivalent(residuals(model2), model1$resid())
})