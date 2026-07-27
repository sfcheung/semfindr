# Print an 'influence_stat' Class Object

Print the content of an 'influence_stat'-class object.

## Usage

``` r
# S3 method for class 'influence_stat'
print(
  x,
  digits = 3,
  what = c("parameters", "fit_measures", "mahalanobis"),
  first = 10,
  sort_parameters_by = c("gcd", "est"),
  sort_fit_measures_by = NULL,
  sort_mahalanobis = TRUE,
  sort_fit_measures_decreasing = TRUE,
  sort_fit_measures_on_absolute = TRUE,
  sort_mahalanobis_decreasing = TRUE,
  ...
)
```

## Arguments

- x:

  An 'influence_stat'-class object.

- digits:

  The number of digits after the decimal. Default is 3.

- what:

  A character vector of the results \#' to be printed, can be one or
  more of the following: `"parameters"`, `"fit_measures"`, and
  `"mahalanobis"`. Default is
  `c("parameters", "fit_measures", "mahalanobis")`.

- first:

  Numeric. If not `NULL`, it prints only the first *k* cases, *k* equal
  to `first`. Default is 10.

- sort_parameters_by:

  String. If it is `"est"`, the cases are sorted individually on each
  columns. If it is `"gcd"`, the default, then cases are sorted by
  generalized Cook's distance or approximate generalized Cook's
  distance, depending on which column is available. If `NULL`, cases are
  not sorted.

- sort_fit_measures_by:

  String. Default is `NULL` and the output of case influence on fit
  measures is not sorted. If set to a column names of case influence on
  fit measures , cases will sorted by these columns. The sorting is done
  on the absolute values if `sort_fit_measures_on_absolute` is `TRUE`,
  and in decreasing order if `decreasing` is `TRUE`. If `decrease` is
  `FALSE`, the order is increasing. If `sort_fit_measures_on_absolute`
  is `FALSE`, the sorting is done on the raw values.

- sort_mahalanobis:

  Logical. If `TRUE`, the default, the cases in the output of
  Mahalanobis distance will be sorted based on Mahalanobis distance. The
  order is determined by `sort_mahalanobis_decreasing`.

- sort_fit_measures_decreasing:

  Logical. Whether cases, if sorted on fit measures, are on decreasing
  order in the output of case influence on fit measures. Default is
  `TRUE`.

- sort_fit_measures_on_absolute:

  Logical. Whether cases, if sorted on fit measures, are sorted on
  absolute values of fit measures. Default is `TRUE`. See
  `sort_fit_measures_by`.

- sort_mahalanobis_decreasing:

  Logical. Whether cases, if sorted on Mahalanobis distance, is on
  decreasing order. Default is `TRUE`.

- ...:

  Optional arguments. Passed to other print methods, such as
  [`print.est_change()`](https://sfcheung.github.io/semfindr/reference/print.est_change.md),
  [`print.fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/print.fit_measures_change.md),
  and
  [`print.md_semfindr()`](https://sfcheung.github.io/semfindr/reference/print.md_semfindr.md).

## Value

`x` is returned invisibly. Called for its side effect.

## Details

This method will print the output of
[`influence_stat()`](https://sfcheung.github.io/semfindr/reference/influence_stat.md)
in a user-friendly way. Users can select the set(s) of output, case
influence on parameter estimates, case influence on fit measures, and
Mahalanobis distance, to be printed. The corresponding print methods of
`est_change`-class objects, `fit_measures_change`-class objects, and
`md_semfindr`-class objects will be called.

## See also

[`influence_stat()`](https://sfcheung.github.io/semfindr/reference/influence_stat.md),
[`print.est_change()`](https://sfcheung.github.io/semfindr/reference/print.est_change.md),
[`print.fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/print.fit_measures_change.md),
[`print.md_semfindr()`](https://sfcheung.github.io/semfindr/reference/print.md_semfindr.md)

## Examples

``` r
library(lavaan)
dat <- pa_dat
# The model
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

# --- Leave-One-Out Approach

# Fit the model n times. Each time with one case removed.
# For illustration, do this only for selected cases.
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.5 second(s).
#> Could be faster if run in parallel.
# Get all default influence stats
out <- influence_stat(fit_rerun)
out
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        a1     a2      b m1~~m1 dv~~dv   gcd
#> 9  -0.048 -0.025 -0.083 -0.033  0.283 0.091
#> 7  -0.119  0.073  0.065 -0.002 -0.040 0.026
#> 8   0.058  0.067  0.028 -0.052 -0.067 0.015
#> 10 -0.055  0.041 -0.077 -0.054  0.038 0.015
#> 5   0.067  0.028  0.033  0.050 -0.066 0.013
#> 3  -0.038 -0.040 -0.030 -0.063 -0.045 0.010
#> 2   0.007  0.003 -0.013 -0.067 -0.058 0.008
#> 6   0.004  0.001  0.010 -0.069 -0.054 0.008
#> 1   0.024 -0.030  0.052 -0.037  0.017 0.006
#> 4  -0.024 -0.003  0.022 -0.051 -0.044 0.006
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
#> 
#> -- Case Influence on Fit Measures --
#> 
#>     chisq    cfi  rmsea    tli
#> 1   0.154 -0.002  0.002 -0.005
#> 2  -0.019  0.001 -0.001  0.003
#> 3  -0.417  0.008 -0.007  0.021
#> 4  -0.154  0.004 -0.003  0.009
#> 5   0.097  0.000  0.001  0.001
#> 6   0.116 -0.001  0.001 -0.003
#> 7  -0.631  0.014 -0.011  0.034
#> 8   0.120  0.002  0.001  0.005
#> 9   0.524 -0.012  0.008 -0.030
#> 10  0.697 -0.013  0.011 -0.033
#> 
#> Note:
#> - All stored cases are displayed.
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
print(out, first = 4)
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        a1     a2      b m1~~m1 dv~~dv   gcd
#> 9  -0.048 -0.025 -0.083 -0.033  0.283 0.091
#> 7  -0.119  0.073  0.065 -0.002 -0.040 0.026
#> 8   0.058  0.067  0.028 -0.052 -0.067 0.015
#> 10 -0.055  0.041 -0.077 -0.054  0.038 0.015
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - Only the first 4 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by generalized Cook's distance.
#> 
#> -- Case Influence on Fit Measures --
#> 
#>    chisq    cfi  rmsea    tli
#> 1  0.154 -0.002  0.002 -0.005
#> 2 -0.019  0.001 -0.001  0.003
#> 3 -0.417  0.008 -0.007  0.021
#> 4 -0.154  0.004 -0.003  0.009
#> 
#> Note:
#> - Only the first 4 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> 
#> -- Mahalanobis Distance --
#> 
#>       md
#> 9  5.304
#> 7  4.017
#> 3  3.787
#> 10 3.104
#> 
#> Note:
#> - Only the first 4 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by Mahalanobis distance in decreasing order.
print(out, what = c("parameters", "fit_measures"))
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        a1     a2      b m1~~m1 dv~~dv   gcd
#> 9  -0.048 -0.025 -0.083 -0.033  0.283 0.091
#> 7  -0.119  0.073  0.065 -0.002 -0.040 0.026
#> 8   0.058  0.067  0.028 -0.052 -0.067 0.015
#> 10 -0.055  0.041 -0.077 -0.054  0.038 0.015
#> 5   0.067  0.028  0.033  0.050 -0.066 0.013
#> 3  -0.038 -0.040 -0.030 -0.063 -0.045 0.010
#> 2   0.007  0.003 -0.013 -0.067 -0.058 0.008
#> 6   0.004  0.001  0.010 -0.069 -0.054 0.008
#> 1   0.024 -0.030  0.052 -0.037  0.017 0.006
#> 4  -0.024 -0.003  0.022 -0.051 -0.044 0.006
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
#> 
#> -- Case Influence on Fit Measures --
#> 
#>     chisq    cfi  rmsea    tli
#> 1   0.154 -0.002  0.002 -0.005
#> 2  -0.019  0.001 -0.001  0.003
#> 3  -0.417  0.008 -0.007  0.021
#> 4  -0.154  0.004 -0.003  0.009
#> 5   0.097  0.000  0.001  0.001
#> 6   0.116 -0.001  0.001 -0.003
#> 7  -0.631  0.014 -0.011  0.034
#> 8   0.120  0.002  0.001  0.005
#> 9   0.524 -0.012  0.008 -0.030
#> 10  0.697 -0.013  0.011 -0.033
#> 
#> Note:
#> - All stored cases are displayed.

# --- Approximate Approach

out_approx <- influence_stat(fit)
out_approx
#> 
#> -- Approximate Standardized Case Influence on Parameter Estimates --
#> 
#>     m1~iv1 m1~iv2  dv~m1 m1~~m1 dv~~dv gcd_approx
#> 16   0.052 -0.038 -0.228 -0.006  0.572      0.372
#> 43  -0.387 -0.249 -0.135  0.201  0.116      0.270
#> 65   0.150  0.189  0.355  0.071  0.148      0.203
#> 85  -0.170  0.211 -0.118  0.315 -0.054      0.187
#> 51   0.405 -0.052  0.094  0.075 -0.046      0.179
#> 34  -0.306 -0.186 -0.110  0.176  0.028      0.163
#> 32  -0.241  0.190 -0.189  0.181 -0.002      0.161
#> 20  -0.234  0.199 -0.140  0.172 -0.034      0.144
#> 18  -0.269  0.035  0.101  0.246 -0.048      0.143
#> 100 -0.001 -0.221 -0.069  0.290 -0.058      0.137
#> 
#> Note:
#> - Changes are approximate standardized raw changes if a case is included.
#> - Only the first 10 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by approximate generalized Cook's distance.
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
#> 
#> -- Mahalanobis Distance --
#> 
#>        md
#> 16 11.530
#> 99 11.312
#> 87 11.091
#> 43 10.181
#> 51  9.869
#> 13  8.476
#> 91  8.078
#> 71  7.757
#> 17  7.555
#> 68  7.472
#> 
#> Note:
#> - Only the first 10 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by Mahalanobis distance in decreasing order.
print(out, first = 8)
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        a1     a2      b m1~~m1 dv~~dv   gcd
#> 9  -0.048 -0.025 -0.083 -0.033  0.283 0.091
#> 7  -0.119  0.073  0.065 -0.002 -0.040 0.026
#> 8   0.058  0.067  0.028 -0.052 -0.067 0.015
#> 10 -0.055  0.041 -0.077 -0.054  0.038 0.015
#> 5   0.067  0.028  0.033  0.050 -0.066 0.013
#> 3  -0.038 -0.040 -0.030 -0.063 -0.045 0.010
#> 2   0.007  0.003 -0.013 -0.067 -0.058 0.008
#> 6   0.004  0.001  0.010 -0.069 -0.054 0.008
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - Only the first 8 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by generalized Cook's distance.
#> 
#> -- Case Influence on Fit Measures --
#> 
#>    chisq    cfi  rmsea    tli
#> 1  0.154 -0.002  0.002 -0.005
#> 2 -0.019  0.001 -0.001  0.003
#> 3 -0.417  0.008 -0.007  0.021
#> 4 -0.154  0.004 -0.003  0.009
#> 5  0.097  0.000  0.001  0.001
#> 6  0.116 -0.001  0.001 -0.003
#> 7 -0.631  0.014 -0.011  0.034
#> 8  0.120  0.002  0.001  0.005
#> 
#> Note:
#> - Only the first 8 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
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
#> 
#> Note:
#> - Only the first 8 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by Mahalanobis distance in decreasing order.
print(out, what = c("parameters", "fit_measures"),
      sort_parameters_by = "est")
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>    id     a1 id     a2 id      b id m1~~m1 id dv~~dv id   gcd
#> 1   7 -0.119  7  0.073  9 -0.083  6 -0.069  9  0.283  9 0.091
#> 2   5  0.067  8  0.067 10 -0.077  2 -0.067  8 -0.067  7 0.026
#> 3   8  0.058 10  0.041  7  0.065  3 -0.063  5 -0.066  8 0.015
#> 4  10 -0.055  3 -0.040  1  0.052 10 -0.054  2 -0.058 10 0.015
#> 5   9 -0.048  1 -0.030  5  0.033  8 -0.052  6 -0.054  5 0.013
#> 6   3 -0.038  5  0.028  3 -0.030  4 -0.051  3 -0.045  3 0.010
#> 7   1  0.024  9 -0.025  8  0.028  5  0.050  4 -0.044  2 0.008
#> 8   4 -0.024  2  0.003  4  0.022  1 -0.037  7 -0.040  6 0.008
#> 9   2  0.007  4 -0.003  2 -0.013  9 -0.033 10  0.038  1 0.006
#> 10  6  0.004  6  0.001  6  0.010  7 -0.002  1  0.017  4 0.006
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute values of change or generalized Cook's distance.
#> 
#> -- Case Influence on Fit Measures --
#> 
#>     chisq    cfi  rmsea    tli
#> 1   0.154 -0.002  0.002 -0.005
#> 2  -0.019  0.001 -0.001  0.003
#> 3  -0.417  0.008 -0.007  0.021
#> 4  -0.154  0.004 -0.003  0.009
#> 5   0.097  0.000  0.001  0.001
#> 6   0.116 -0.001  0.001 -0.003
#> 7  -0.631  0.014 -0.011  0.034
#> 8   0.120  0.002  0.001  0.005
#> 9   0.524 -0.012  0.008 -0.030
#> 10  0.697 -0.013  0.011 -0.033
#> 
#> Note:
#> - All stored cases are displayed.
```
