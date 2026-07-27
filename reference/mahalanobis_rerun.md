# Mahalanobis Distance on All Observed Variables

Computes the Mahalanobis distance for each case on all observed
variables in a model.

## Usage

``` r
mahalanobis_rerun(
  fit,
  emNorm_arg = list(estimate.worst = FALSE, criterion = 1e-06)
)
```

## Arguments

- fit:

  It can be the output from `lavaan`, such as
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html), or the
  output from
  [`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).

- emNorm_arg:

  No longer used. Kept for backward compatibility.

## Value

A `md_semfindr`-class object, which is a one-column matrix (a column
vector) of the Mahalanobis distance for each case. The row names are the
case identification values used in
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).
A print method is available for user-friendly output.

## Details

`mahalanobis_rerun()` gets a
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
or [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html)
output and computes the Mahalanobis distance for each case on all
observed variables.

If there are no missing values,
[`stats::mahalanobis()`](https://rdrr.io/r/stats/mahalanobis.html) will
be used to compute the Mahalanobis distance.

If there are missing values on the observed predictors, the means and
variance-covariance matrices will be estimated by maximum likelihood
using [`lavaan::lavCor()`](https://rdrr.io/pkg/lavaan/man/lavCor.html).
The estimates will be passed to
[`modi::MDmiss()`](https://rdrr.io/pkg/modi/man/MDmiss.html) to compute
the Mahalanobis distance.

Supports both single-group and multiple-group models. For multiple-group
models, the Mahalanobis distance for each case is computed using the
means and covariance matrix of the group this case belongs to. (Support
for multiple-group models available in 0.1.4.8 and later version).

## References

Mahalanobis, P. C. (1936). On the generalized distance in statistics.
*Proceedings of the National Institute of Science of India, 2*, 49-55.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.

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
# Fit the model n times. Each time with one case removed.
# For illustration, do this only for selected cases.
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.48 second(s).
#> Could be faster if run in parallel.
# Compute the Mahalanobis distance for each case
out <- mahalanobis_rerun(fit_rerun)
# Results excluding a case, for the first few cases
head(out)
#>          md
#> 1 1.9107778
#> 2 0.4442464
#> 3 3.7867385
#> 4 1.0653437
#> 5 1.9803351
#> 6 0.2875484
# Compute the Mahalanobis distance using stats::mahalanobis()
md1 <- stats::mahalanobis(dat, colMeans(dat), stats::cov(dat))
# Compare the results
head(md1)
#> [1] 1.9107778 0.4442464 3.7867385 1.0653437 1.9803351 0.2875484

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

fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.6 second(s).
#> Could be faster if run in parallel.
mahalanobis_rerun(fit_rerun)
#> 
#> -- Mahalanobis Distance --
#> 
#>        md
#> 3  21.492
#> 2   8.802
#> 1   8.178
#> 10  8.090
#> 9   6.552
#> 5   5.135
#> 8   3.415
#> 7   2.470
#> 6   2.382
#> 4   2.142
#> 
#> Note:
#> - All stored cases are displayed.
#> - Cases sorted by Mahalanobis distance in decreasing order.

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
fit <- lavaan::cfa(mod, dat)

fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.77 second(s).
#> Could be faster if run in parallel.
mahalanobis_rerun(fit_rerun)
#> 
#> -- Mahalanobis Distance --
#> 
#>        md
#> 6  17.521
#> 9  16.870
#> 4  13.337
#> 1  13.332
#> 3   9.792
#> 7   9.485
#> 2   8.445
#> 5   8.319
#> 8   5.332
#> 10  1.904
#> 
#> Note:
#> - All stored cases are displayed.
#> - Cases sorted by Mahalanobis distance in decreasing order.

```
