# Print a 'md_semfindr' Class Object

Print the content of a 'md_semfindr'-class object.

## Usage

``` r
# S3 method for class 'md_semfindr'
print(x, digits = 3, first = 10, sort = TRUE, decreasing = TRUE, ...)
```

## Arguments

- x:

  An 'md_semfindr'-class object.

- digits:

  The number of digits after the decimal. Default is 3.

- first:

  Numeric. If not `NULL`, it prints only the first *k* cases, *k* equal
  to `first`. Default is 10.

- sort:

  Logical. If `TRUE`, the default, the cases will be sorted based on
  Mahalanobis distance. The order is determined by `decreasing`.

- decreasing:

  Logical. Whether cases, if sorted, is on decreasing order. Default is
  `TRUE`.

- ...:

  Other arguments. They will be ignored.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The print method for the 'md_semfindr'-class object, returned by
[`mahalanobis_rerun()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_rerun.md)
or
[`mahalanobis_predictors()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_predictors.md).
This method will print the output with the option to sort the cases.

## See also

[`mahalanobis_rerun()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_rerun.md),
[`mahalanobis_predictors()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_predictors.md)

## Examples

``` r

library(lavaan)
dat <- pa_dat
# The model
mod <-
"
m1 ~ a1 * iv1 + a2 * iv2
dv ~ b * m1
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
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 6.711
#>   Degrees of freedom                                 2
#>   P-value (Chi-square)                           0.035
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
#>     iv1       (a1)    0.215    0.106    2.036    0.042
#>     iv2       (a2)    0.522    0.099    5.253    0.000
#>   dv ~                                                
#>     m1         (b)    0.517    0.106    4.895    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m1                0.903    0.128    7.071    0.000
#>    .dv                1.321    0.187    7.071    0.000
#> 
# Fit the model n times. Each time with one case removed.
# For illustration, do this only for selected cases.
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.46 second(s).
#> Could be faster if run in parallel.
# Compute the Mahalanobis distance for each case
out <- mahalanobis_rerun(fit_rerun)
out
#> 
#> -- Mahalanobis Distance --
#> 
#>       md
#> 9  5.304
#> 7  4.017
#> 3  3.787
#> 10 3.104
#> 8  3.005
#> 5  1.980
#> 1  1.911
#> 4  1.065
#> 2  0.444
#> 6  0.288
#> 
#> Note:
#> - All stored cases are displayed.
#> - Cases sorted by Mahalanobis distance in decreasing order.
print(out, first = 3)
#> 
#> -- Mahalanobis Distance --
#> 
#>      md
#> 9 5.304
#> 7 4.017
#> 3 3.787
#> 
#> Note:
#> - Only the first 3 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by Mahalanobis distance in decreasing order.
```
