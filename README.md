# RLab4

This package contains functions to fit and predict models with linear regression and ridge regression models. 
The project was initialized as a task in a course, and has been improved as a part of another course.

## Getting started
Before installing the package, ```devtools``` has to be installed and loaded. Do this by running 

```
install.packages("devtools")
library(devtools)
```
## Installing the package
Install the package by running
```
devtools::install_github("https://github.com/carsv733/RLab4", 
                          subdir = "RLab4", build_vignettes = TRUE, 
                          dependencies = TRUE)
```
Setting ```dependencies = TRUE``` installs all required packages.

## Package and method details
Package details can be obtained by running

```
?RLab4
```
Details of the package classes and their methods (```linreg``` and ```ridgereg```) can be found in their corresponding manuals. Obtain the manuals by running

```
?linreg
```
and
```
?ridgereg
```

## Examples
Examples are found in two different vignettes. The ```ridgereg``` and ```linreg``` vignettes are obtained by running

```
vignette("ridgereg") #Ridge regression
vignette("RLab4") #Linear regression
```

## Testing
The tests are focused on function input control such as warnings or errors, and output control such as checking that the functions return the expected classes, object sizes or numbers. The tests are run automatically when installing the package. They can, however, be run manually using the ```testthat``` package:
```
install.packages("testthat")
library("testthat")
result <- test_package("RLab4") 
```
Details of the tests will then be represented as a list in ```result```.

## Note to TA
Please ignore the implemented theme function and the ```flight_delay``` file - these are files not relevant 
to this software project, however, I do not want to delete them.
