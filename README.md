# 732A94_Bonus_Lab

This package is made for 732A94 Advanced Programming in R course.

<!-- badges: start -->
[![R-CMD-check](https://github.com/Hytreueroot/732A94_Lab4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Hytreueroot/732A94_Lab4/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


## Description
A package to handle linear regression models with using linear algebra. Implement a RC class to handle special functions.
Also package take a formula object as well as a dataset and return a ridgereg object. Implement a RC class to handle special functions.

### print()
This function print out the coefficients and coefficient names, similar as done by the lm class.

```ruby
mod_object$print()
```

### plot()
This function print out two graph ("Residuals vs Fitted" and "Scale-Location") using ggplot2.
```ruby
mod_object$plot()
```

### resid()
This function print out the vector of residuals $\hat{e}$.

```ruby
mod_object$resid()
```

### pred()
This function print out the predicted values $\hat{y}$.

```ruby
mod_object$pred()
```

### coef()
This function print out the coefficients as a named vector.

```ruby
mod_object$coef()
```

### summary()
This function print out a similar print out as printed for lm objects. It presents the coefficients with their standard error, t-value and p-value as well as the estimate of $\hat{σ}$ and the degrees of freedom in the model.

```ruby
mod_object$summary()
```


## Installation
Install the package using the following code in R.

```ruby
devtools::install_github("Hytreueroot/732A94_Lab4", build_vignettes=TRUE)
```
For browse vignette:
```ruby
browseVignettes(package="Lab4")
```

## Contributors
SILA KILICOGLU        **silki753**          silki573@student.liu.se <br>
SAMI FURKAN YILDIRIM  **samyi572**          samyi572@student.liu.se
