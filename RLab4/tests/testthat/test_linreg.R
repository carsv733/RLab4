
test_that("Return right class", {
  data(iris)
  model <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  expect_that(model, is_a("linreg"))
})


test_that("Return right object", {
  formula <- Sepal.Length ~ Petal.Length
  expect_that(formula, is_a("formula"))
})
