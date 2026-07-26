# Implied Scores of Observed Outcome Variables

Gets a [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html)
output and computes the implied scores of observed outcome variables.

## Usage

``` r
implied_scores(fit, output = "matrix", skip_all_checks = FALSE)
```

## Arguments

- fit:

  The output from
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html), such
  as [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- output:

  Output type. If `"matrix"`, the default, the output will be combined
  to one matrix, with cases ordered as in the original dataset (after
  listwise deletion, if used). If `"list"`, the a list of matrices is
  returned, even if the model has only one group.

- skip_all_checks:

  If `TRUE`, skips all checks and allows users to run this function on
  any object of `lavaan` class. For users to experiment this and other
  functions on models not officially supported. Default is `FALSE`.

## Value

A matrix of the implied scores if `output` is `"matrix"`. If `output` is
`"list"`, a list of matrices of the implied scores.

## Details

The implied scores for each observed outcome variable (the `y`-variables
or the endogenous variables) are simply computed in the same way the
predicted scores in a linear regression model are computed.

Currently it supports only single-group and multiple-group path analysis
models with only observed variables. (Support for multiple-group models
available in 0.1.4.8 and later version).

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.

## Examples

``` r
library(lavaan)
dat <- pa_dat
# For illustration, select only the first 50 cases
dat <- dat[1:50, ]
# The model
mod <-
"
m1 ~ iv1 + iv2
dv ~ m1
"
# Fit the model
fit <- lavaan::sem(mod, dat)
summary(fit)
#> lavaan 0.7-2 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                            50
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 1.768
#>   Degrees of freedom                                 2
#>   P-value (Chi-square)                           0.413
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   m1 ~                                                
#>     iv1              -0.159    0.166   -0.954    0.340
#>     iv2               0.525    0.162    3.241    0.001
#>   dv ~                                                
#>     m1                0.350    0.161    2.169    0.030
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m1                0.901    0.180    5.000    0.000
#>    .dv                1.423    0.285    5.000    0.000
#> 

# Compute the implied scores for `m1` and `dv`
fit_implied_scores <- implied_scores(fit)
head(fit_implied_scores)
#>            m1          dv
#> 1 -0.27796469  0.11231710
#> 2  0.02685371  0.05380020
#> 3  0.44378891  0.12306764
#> 4 -0.04555044 -0.19799762
#> 5 -0.02779787 -0.56271155
#> 6 -0.01323771  0.01100961

```
