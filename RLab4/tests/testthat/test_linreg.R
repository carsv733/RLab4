
test_that("function returns a linreg object", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model, is_a("linreg"))
})

test_that("function returns formula as a formula object", {
  formula <- Sepal.Length ~ Petal.Length
  expect_that(formula, is_a("formula"))
})

test_that("function returns input data as a data frame", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$data, is_a("data.frame"))
})

test_that("function returns coefficients as a matrix", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$betaHat, is_a("matrix"))
})

test_that("function returns predictions as a matrix", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$yHat, is_a("matrix"))
})

test_that("function returns residuals as a matrix", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$res, is_a("matrix"))
})

test_that("function returns degrees of freedom as an integer", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$df, is_a("integer"))
})

test_that("function returns residual variance as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$varRes, is_a("numeric"))
})

test_that("function returns coefficient variance as a matix", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$varCoef, is_a("matrix"))
})

test_that("function returns normalized coefficients as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$tBeta2, is_a("numeric"))
})

test_that("function returns p-value as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$p, is_a("numeric"))
})

test_that("function returns standard deviation of residuals as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$rstand2, is_a("numeric"))
})

test_that("function returns standard deviation of coefficients as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$cstand2, is_a("numeric"))
})