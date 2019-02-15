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
 \item{resid}{
a vector of the residuals.
}
 \item{pred}{
a vector of the predicted values.
}
 \item{plot}{
a plot of residuals vs fitted values and a plot of standardized residuals vs fitted values.
}
 \item{summary}{
a list containing the coefficients with their standard errors, t-value and p-value, degrees of freedom and the estimate of sigma.
}
}

\keyword{linearregression}
