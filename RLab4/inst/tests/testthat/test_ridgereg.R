
test_that("Return right class", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Petal.Length+Petal.Width,iris,lambda=0.5)
  expect_that(model, is_a("ridgereg"))
})


test_that("Insert right object", {
  formula <- Sepal.Length ~ Petal.Length
  expect_that(formula, is_a("formula"))
})

