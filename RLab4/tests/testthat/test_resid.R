test_that("Return vector of residuals", {
  data(iris)
  model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)
 all.equal(residuals(model2), is_equivalent_to(model1$resid()))
})