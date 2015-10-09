\name{ridgereg}
\alias{ridgereg}
\title{
Ridge Regression
}
\description{
Fitting linear models
}
\usage{
ridgereg(formula,data,lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an argument of class "formula".}
  \item{data}{an optional data frame.}
  \item{lambda}{A ridge regression parameter. May be a vector.}
}

\value{
ridgereg returns an object of class "ridgereg".

ridgereg contains the following components:
 \item{coef}{
a named vector of coefficients.
}
 \item{predict}{
a vector of the predicted values.
}
 \item{print}{
a vector of coefficients and coefficient names.
}
}

