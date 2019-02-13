
test_that("Return right class", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model, is_a("ridgereg"))
})


test_that("Insert right object", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$formula, is_a("formula"))
})

test_that("function returns input data as a data frame", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$data, is_a("data.frame"))
})

test_that("function returns coefficients as a matrix", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$betaHat, is_a("matrix"))
})

test_that("function returns predictions as a matrix", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$yHat, is_a("matrix"))
})

test_that("function returns residuals as a matrix", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$res, is_a("matrix"))
})

test_that("function returns degrees of freedom as an integer", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$df, is_a("integer"))
})

test_that("function returns residual variance as a numeric", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$varRes, is_a("numeric"))
})

test_that("function returns coefficient variance as a matix", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$varCoef, is_a("matrix"))
})

test_that("function returns normalized coefficients as a numeric", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$tBeta2, is_a("numeric"))
})

test_that("function returns p-value as a numeric", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$p, is_a("numeric"))
})

test_that("function returns standard deviation of residuals as a numeric", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$rstand2, is_a("numeric"))
})

test_that("function returns standard deviation of coefficients as a numeric", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model$cstand2, is_a("numeric"))
})

#Message tests
test_that("function returns a message when not fed any data", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_message(model$predict(), "No data provided, using original input.")
})

test_that("function throws an error when not fed a formula", {
  data(iris)
  form <- "Sepal.Length ~ Petal.Length+Petal.Width"
  expect_error(ridgereg(form,iris,lambda=0.5), "The formula provided is not a formula.")
})

test_that("function throws an error when not fed a data frame", {
  data(iris)
  expect_error(ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,as.list(iris),lambda=0.5), "The data provided is not a data frame.")
})

test_that("function throws an error when not fed a numeric lambda", {
  data(iris)
  expect_error(ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda="0.5"), "The lambda provided is not a numeric.")
})

