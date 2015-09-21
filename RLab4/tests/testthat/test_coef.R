data(iris)
model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)

test_that("Return vector of named coefficients", {
  expect_that(coefficients(model2), equals(model1$coef()))
})


