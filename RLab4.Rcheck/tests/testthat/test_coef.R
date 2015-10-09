test_that("Return vector of named coefficients", {
  data(iris)
  model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)
  all.equal(coefficients(model2), is_equivalent_to(model1$coef()))
})


