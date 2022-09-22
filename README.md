<!-- badges: start -->
[![R-CMD-check](https://github.com/AlanCT94/Lab04/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlanCT94/Lab04/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
# Lab04

Package that contains functions to do multiple linear regression. 

The function linreg estimates the least squares solution through direct matrix calculation while the function linreg_qr estimates the solution using QR factorisation. The package further contains functions to view the results: print, that prints the model call and estimated coefficients, summary, that provides a summary table of the estimated coefficients along with their standard errors, t-values and p-values. There is also a plot function for model diagnostics. Further there are functions that return the coefficients (coef), predicted values (pred) and residuals (residuals).

## Installation

You can install the development version of this package from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AlanCT94/Lab04")
```
