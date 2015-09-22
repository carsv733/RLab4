\name{linreg}
\alias{linreg}
\docType{data}
\title{
Linear regression
}
\description{
Fitting linear models.
}
\usage{
linreg(formula,data)
}
\arguments{
  \item{formula}{
an argument of class "formula".
}
  \item{data}{
an optional data frame.
}
}
\value{
linreg returns an object of class "linreg".

linreg contains the following components:
 \item{coef}{
a named vector of coefficients.
}
 \item{pred}{
a vector of the predicted values.
}
 \item{resid}{
a vector of the residuals.
}
 \item{summary}{
a list containing the coefficients with their standard error, t-value and p-value, the estimate of sigma and the degrees of freedom.
}
 \item{print}{
a vector of coefficients and coefficient names.
}
 \item{plot}{
a plot of residuals vs fitted and a plot of standardized residuals vs fitted.
}
}

\keyword{linearregression}
