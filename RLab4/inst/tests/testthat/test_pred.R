test_that("predicted values match predicted values of built-in function", {
  data(iris)
  model1 <- linreg(Sepal.Length ~ Petal.Length, data=iris)
  model2 <- lm(Sepal.Length ~ Petal.Length, data=iris)
  expect_equivalent(fitted.values(model2), model1$pred())
})