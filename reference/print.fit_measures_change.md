# Print a 'fit_measures_change' Class Object

Print the content of a 'fit_measures_change'-class object.

## Usage

``` r
# S3 method for class 'fit_measures_change'
print(
  x,
  digits = 3,
  first = 10,
  sort_by = NULL,
  decreasing = TRUE,
  absolute = TRUE,
  ...
)
```

## Arguments

- x:

  An 'fit_measures_change'-class object.

- digits:

  The number of digits after the decimal. Default is 3.

- first:

  Numeric. If not `NULL`, it prints only the first *k* cases, *k* equal
  to `first`. Default is 10.

- sort_by:

  String. Default is `NULL` and the output is not sorted. If set to a
  column names of `x`, cases will sorted by this columns. The sorting is
  done on the absolute values if `absolute` is `TRUE`, and in decreasing
  order if `decreasing` is `TRUE`. If `decrease` is `FALSE`, the order
  is increasing. If `absolute` is `FALSE`, the sorting is done on the
  raw values.

- decreasing:

  Logical. Whether cases, if sorted, is on decreasing order. Default is
  `TRUE`. See `sort_by`.

- absolute:

  Logical. Whether cases, if sorted, are sorted on absolute values.
  Default is `TRUE`. See `sort_by`.

- ...:

  Other arguments. They will be ignored.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

All the functions on case influence on fit measures,
[`fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change.md)
and
[`fit_measures_change_approx()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change_approx.md),
return an `fit_measures_change`-class object. This method will print the
output, with the option to sort the cases.

## See also

[`fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change.md),
[`fit_measures_change_approx()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change_approx.md)

## Examples

``` r

library(lavaan)

# A path model

dat <- pa_dat
mod <-
"
m1 ~ a1 * iv1 + a2 * iv2
dv ~ b * m1
a1b := a1 * b
a2b := a2 * b
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
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     a1b               0.111    0.059    1.880    0.060
#>     a2b               0.270    0.075    3.581    0.000
#> 
#> lavaan NOTE:  
#>    Standard errors and confidence intervals of the (nonlinear) defined (:=) 
#>    parameters are based on the first-order delta method; for strongly 
#>    nonlinear definitions, se.def = "mc" (Monte Carlo) or se = "bootstrap" may 
#>    be more accurate.

# Case influence
out <- fit_measures_change_approx(fit)
out
#> 
#> -- Approximate Case Influence on Fit Measures --
#> 
#>     chisq    cfi  rmsea    tli
#> 1   0.160 -0.002  0.002 -0.005
#> 2  -0.019  0.001 -0.001  0.003
#> 3  -0.389  0.008 -0.007  0.019
#> 4  -0.151  0.004 -0.003  0.009
#> 5   0.097  0.000  0.001  0.001
#> 6   0.116 -0.001  0.001 -0.003
#> 7  -0.596  0.013 -0.010  0.032
#> 8   0.119  0.002  0.001  0.005
#> 9   0.543 -0.012  0.008 -0.031
#> 10  0.703 -0.013  0.011 -0.033
#> 
#> Note:
#> - Only the first 10 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
print(out, sort_by = "chisq", first = 5)
#> 
#> -- Approximate Case Influence on Fit Measures --
#> 
#>     chisq    cfi  rmsea    tli
#> 91  1.846 -0.035  0.033 -0.089
#> 25  1.621 -0.032  0.029 -0.080
#> 43  1.392 -0.031  0.024 -0.078
#> 17 -1.389  0.023 -0.022  0.058
#> 16 -1.283  0.016 -0.021  0.039
#> 
#> Note:
#> - Only the first 5 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by chisq in decreasing order on absolute values.

fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(2, 3, 5, 7))#'
#> The expected CPU time is 0.19 second(s).
#> Could be faster if run in parallel.
out <- fit_measures_change(fit_rerun)
out
#> 
#> -- Case Influence on Fit Measures --
#> 
#>    chisq   cfi  rmsea   tli
#> 2 -0.019 0.001 -0.001 0.003
#> 3 -0.417 0.008 -0.007 0.021
#> 5  0.097 0.000  0.001 0.001
#> 7 -0.631 0.014 -0.011 0.034
#> 
#> Note:
#> - All stored cases are displayed.
print(out, sort_by = "chisq", first = 5)
#> 
#> -- Case Influence on Fit Measures --
#> 
#>    chisq   cfi  rmsea   tli
#> 7 -0.631 0.014 -0.011 0.034
#> 3 -0.417 0.008 -0.007 0.021
#> 5  0.097 0.000  0.001 0.001
#> 2 -0.019 0.001 -0.001 0.003
#> 
#> Note:
#> - All stored cases are displayed.
#> - Cases sorted by chisq in decreasing order on absolute values.
```
