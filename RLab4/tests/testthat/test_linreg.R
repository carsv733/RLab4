data(iris)
model <- linreg(Sepal.Length ~ Petal.Length, data=iris)

test_that("Return right class", {
  expect_that(model, is_a("linreg"))
})



formula is formula object
test_that("Return right object", {
  expect_that(formula, is_a("formula"))
})
