# Case Influence on Parameter Estimates (Approximate DFTHETA)

Gets a [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html)
output and computes the approximate changes in selected parameters for
each case if included.

## Usage

``` r
est_change_raw_approx(
  fit,
  parameters = NULL,
  case_id = NULL,
  allow_inadmissible = FALSE,
  skip_all_checks = FALSE
)
```

## Arguments

- fit:

  The output from
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers (e.g.,
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)).

- parameters:

  A character vector to specify the selected parameters. Each parameter
  is named as in `lavaan` syntax, e.g., `x ~ y` or `x ~~ y`, as appeared
  in the columns `lhs`, `op`, and `rhs` in the output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  Supports specifying an operator to select all parameters with these
  operators: `~`, `~~`, `=~`, and `~1`. This vector can contain both
  parameter names and operators. More details can be found in the help
  of
  [`pars_id()`](https://sfcheung.github.io/semfindr/reference/pars_id.md).
  If omitted or `NULL`, the default, changes on all free parameters will
  be computed.

- case_id:

  If it is a character vector of length equals to the number of cases
  (the number of rows in the data in `fit`), then it is the vector of
  case identification values. If it is `NULL`, the default, then
  `case.idx` used by `lavaan` functions will be used as case
  identification values.

- allow_inadmissible:

  If `TRUE`, accepts a fit object with inadmissible results (i.e.,
  `post.check` from
  [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  is `FALSE`). Default is `FALSE`.

- skip_all_checks:

  If `TRUE`, skips all checks and allows users to run this function on
  any object of `lavaan` class. For users to experiment this and other
  functions on models not officially supported. Default is `FALSE`.

## Value

An `est_change`-class object, which is matrix with the number of columns
equals to the number of requested parameters, and the number of rows
equals to the number of cases. The row names are case identification
values. The elements are the raw differences. A print method is
available for user-friendly output.

## Details

For each case, `est_change_raw_approx()` computes the approximate
differences in the estimates of selected parameters with and without
this case:

(Estimate with all case) - (Estimate without this case).

The change is the approximate raw change (an approximation of DFTHETA).
The change is *not* divided by the standard error of an estimate (hence
"raw" in the function name). This is a measure of the influence of a
case on the parameter estimates if it is included.

If the value of a case is positive, including the case increases an
estimate.

If the value of a case is negative, including the case decreases an
estimate.

The model is not refitted. Therefore, the result is only an
approximation of that of
[`est_change_raw()`](https://sfcheung.github.io/semfindr/reference/est_change_raw.md).
However, this approximation is useful for identifying potentially
influential cases when the sample size is very large or the model takes
a long time to fit. This function can be used to identify potentially
influential cases quickly and then select them to conduct the
leave-one-out sensitivity analysis using
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
and
[`est_change_raw()`](https://sfcheung.github.io/semfindr/reference/est_change_raw.md).

Unlike
[`est_change_raw()`](https://sfcheung.github.io/semfindr/reference/est_change_raw.md),
it does not yet support computing the changes for the standardized
solution.

For the technical details, please refer to the vignette on this
approach:
[`vignette("casewise_scores", package = "semfindr")`](https://sfcheung.github.io/semfindr/articles/casewise_scores.md)

The approximate approach supports a model with equality constraints
(available in 0.1.4.8 and later version).

Supports both single-group and multiple-group models. (Support for
multiple-group models available in 0.1.4.8 and later version).

## Author

Idea by Mark Hok Chio Lai <https://orcid.org/0000-0002-9196-7406>,
implemented by Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.

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
#> lavaan 0.6-21 ended normally after 1 iteration
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
# Compute the approximate changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, the approximate case influence on parameter estimates.
out_approx <- est_change_raw_approx(fit)
head(out_approx)
#>          m1~iv1        m1~iv2        dv~m1       m1~~m1       dv~~dv
#> 1  0.0025826195 -2.987160e-03  0.005478785 -0.004783799  0.003015677
#> 2  0.0007581403  3.411181e-04 -0.001383390 -0.008744342 -0.011010937
#> 3 -0.0039693069 -3.914928e-03 -0.003154241 -0.008240420 -0.008525904
#> 4 -0.0025450320 -3.118916e-04  0.002296162 -0.006671599 -0.008331854
#> 5  0.0070027557  2.748024e-03  0.003450422  0.006144848 -0.012529327
#> 6  0.0004248632  9.671545e-05  0.001008609 -0.008958217 -0.010293608
# Fit the model several times. Each time with one case removed.
# For illustration, do this only for 10 selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.48 second(s).
#> Could be faster if run in parallel.
# Compute the changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, the case influence on the parameter estimates.
out <- est_change_raw(fit_rerun)
out
#> 
#> -- Case Influence on Parameter Estimates --
#> 
#>    id m1~iv1 id m1~iv2 id  dv~m1 id m1~~m1 id dv~~dv id    a1b id    a2b
#> 1   7 -0.013  7  0.007  9 -0.009  6 -0.009  9  0.051  7 -0.005  7  0.007
#> 2   5  0.007  8  0.007 10 -0.008  2 -0.009  8 -0.013 10 -0.005  9 -0.006
#> 3   8  0.006 10  0.004  7  0.007  3 -0.008  5 -0.013  9 -0.005  8  0.005
#> 4  10 -0.006  3 -0.004  1  0.005 10 -0.007  2 -0.011  5  0.004  3 -0.004
#> 5   9 -0.005  1 -0.003  5  0.004  8 -0.007  6 -0.010  8  0.004  5  0.003
#> 6   3 -0.004  5  0.003  3 -0.003  4 -0.007  3 -0.008  3 -0.003 10 -0.002
#> 7   1  0.003  9 -0.003  8  0.003  5  0.006  4 -0.008  1  0.003  1  0.001
#> 8   4 -0.003  2  0.000  4  0.002  1 -0.005  7 -0.008  4 -0.001  4  0.001
#> 9   2  0.001  4  0.000  2 -0.001  9 -0.004 10  0.007  6  0.000  6  0.001
#> 10  6  0.000  6  0.000  6  0.001  7  0.000  1  0.003  2  0.000  2 -0.001
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.
# Compare the results
plot(out_approx[1:10, 1], out[, 1])
abline(a = 0, b = 1)

plot(out_approx[1:10, 5], out[, 5])
abline(a = 0, b = 1)


# A CFA model
dat <- cfa_dat
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f1 ~~ f2
"
# Fit the model
fit <- lavaan::cfa(mod, dat)
summary(fit)
#> lavaan 0.6-21 ended normally after 37 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        13
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                12.027
#>   Degrees of freedom                                 8
#>   P-value (Chi-square)                           0.150
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 =~                                               
#>     x1                1.000                           
#>     x2                0.767    0.225    3.405    0.001
#>     x3                1.047    0.296    3.542    0.000
#>   f2 =~                                               
#>     x4                1.000                           
#>     x5                2.114    0.869    2.431    0.015
#>     x6                0.992    0.377    2.635    0.008
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 ~~                                               
#>     f2                0.171    0.091    1.884    0.060
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x1                0.841    0.221    3.802    0.000
#>    .x2                1.214    0.208    5.823    0.000
#>    .x3                1.018    0.251    4.064    0.000
#>    .x4                1.103    0.186    5.918    0.000
#>    .x5                0.993    0.437    2.270    0.023
#>    .x6                0.882    0.158    5.575    0.000
#>     f1                0.708    0.262    2.703    0.007
#>     f2                0.250    0.151    1.659    0.097
#> 
# Compute the approximate changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, approximate case influence on parameter estimates.
# Compute changes for free loadings only.
out_approx <- est_change_raw_approx(fit,
                                    parameters = "=~")
head(out_approx)
#>         f1=~x2       f1=~x3       f2=~x5        f2=~x6
#> 1  0.001920110  0.010740746  0.004541900 -0.0144098708
#> 2  0.058933568  0.073874875  0.057340739 -0.0434930940
#> 3 -0.144211320 -0.004047592 -0.436403769 -0.0108782714
#> 4 -0.009547216 -0.011335796 -0.000486256  0.0022414847
#> 5  0.004914233 -0.021463799  0.006004056 -0.0086035844
#> 6  0.002934830  0.018952764 -0.016648522  0.0001674064

# A latent variable model
dat <- sem_dat
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~   a * f1
f3 ~   b * f2
ab := a * b
"
# Fit the model
fit <- lavaan::sem(mod, dat)
summary(fit)
#> lavaan 0.6-21 ended normally after 37 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        20
#> 
#>   Number of observations                           200
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                41.768
#>   Degrees of freedom                                25
#>   P-value (Chi-square)                           0.019
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 =~                                               
#>     x1                1.000                           
#>     x2                0.590    0.145    4.054    0.000
#>     x3                0.808    0.168    4.812    0.000
#>   f2 =~                                               
#>     x4                1.000                           
#>     x5                0.730    0.099    7.400    0.000
#>     x6                0.429    0.083    5.166    0.000
#>   f3 =~                                               
#>     x7                1.000                           
#>     x8                2.019    0.589    3.426    0.001
#>     x9                2.747    0.788    3.486    0.000
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f2 ~                                                
#>     f1         (a)    1.115    0.233    4.788    0.000
#>   f3 ~                                                
#>     f2         (b)    0.206    0.061    3.394    0.001
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x1                1.183    0.173    6.831    0.000
#>    .x2                1.129    0.127    8.909    0.000
#>    .x3                1.027    0.134    7.667    0.000
#>    .x4                0.833    0.173    4.812    0.000
#>    .x5                1.078    0.140    7.714    0.000
#>    .x6                1.234    0.132    9.367    0.000
#>    .x7                1.056    0.112    9.428    0.000
#>    .x8                1.042    0.139    7.478    0.000
#>    .x9                1.077    0.197    5.470    0.000
#>     f1                0.658    0.190    3.474    0.001
#>    .f2                0.647    0.215    3.010    0.003
#>    .f3                0.062    0.035    1.771    0.077
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     ab                0.230    0.079    2.895    0.004
#> 
# Compute the approximate changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, the approximate case influence on parameter estimates.
# Compute changes for structural paths only
out_approx <- est_change_raw_approx(fit,
                                    parameters = c("~"))
head(out_approx)
#>          f2~f1         f3~f2
#> 1 -0.001088313 -0.0078430905
#> 2  0.010965561 -0.0055003320
#> 3 -0.031284855 -0.0054784700
#> 4  0.060426691  0.0002338238
#> 5 -0.014994222 -0.0009711227
#> 6 -0.002126077 -0.0009193544

```
