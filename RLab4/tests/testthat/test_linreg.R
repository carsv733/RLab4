
test_that("function returns a linreg object", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model, is_a("linreg"))
})

test_that("function returns formula as a formula object", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$formula, is_a("formula"))
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
  expect_that(model$t_Beta, is_a("numeric"))
})

test_that("function returns p-value as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$p, is_a("numeric"))
})

test_that("function returns standard deviation of residuals as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$r_stand, is_a("numeric"))
})

test_that("function returns standard deviation of coefficients as a numeric", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model$c_stand, is_a("numeric"))
})

# Message tests
test_that("function throws an error when not fed a formula", {
  data(iris)
  form <- "Sepal.Length ~ Petal.Length+Petal.Width"
  expect_error(linreg(form,iris), "The formula provided is not a formula.")
})

test_that("function throws an error when not fed a data frame", {
  data(iris)
  expect_error(linreg(Sepal.Length ~ Petal.Length+Petal.Width,as.list(iris)), "The data provided is not a data frame.")
})
