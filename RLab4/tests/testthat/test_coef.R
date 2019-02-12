test_that("Return vector of named coefficients", {
  data(iris)
  model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)
  expect_equivalent(coefficients(model2), model1$coef())
})


