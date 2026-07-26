# Mahalanobis Distance On Observed Predictors

Gets a
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
or [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html)
output and computes the Mahalanobis distance for each case using only
the observed predictors.

## Usage

``` r
mahalanobis_predictors(
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
vector) of the Mahalanobis distance for each case. The number of rows
equals to the number of cases in the data stored in the fit object. A
print method is available for user-friendly output.

## Details

For each case, `mahalanobis_predictors()` computes the Mahalanobis
distance of each case on the observed predictors.

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

Béguin, C., & Hulliger, B. (2004). Multivariate outlier detection in
incomplete survey data: The epidemic algorithm and transformed rank
correlations. *Journal of the Royal Statistical Society: Series A
(Statistics in Society)*, 167(2), 275-294.

Mahalanobis, P. C. (1936). On the generalized distance in statistics.
*Proceedings of the National Institute of Science of India, 2*, 49-55.

Schafer, J.L. (1997) *Analysis of incomplete multivariate data*. Chapman
& Hall/CRC Press.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.

## Examples

``` r
library(lavaan)
dat <- pa_dat
# For illustration, select only the first 50 cases.
dat <- dat[1:50, ]
# The model
mod <-
"
m1 ~ a1 * iv1 +  a2 * iv2
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
#>     iv1       (a1)   -0.159    0.166   -0.954    0.340
#>     iv2       (a2)    0.525    0.162    3.241    0.001
#>   dv ~                                                
#>     m1         (b)    0.350    0.161    2.169    0.030
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m1                0.901    0.180    5.000    0.000
#>    .dv                1.423    0.285    5.000    0.000
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     a1b              -0.056    0.064   -0.873    0.382
#>     a2b               0.184    0.102    1.803    0.071
#> 
#> lavaan NOTE:  
#>    Standard errors and confidence intervals of the (nonlinear) defined (:=) 
#>    parameters are based on the first-order delta method; for strongly 
#>    nonlinear definitions, se.def = "mc" (Monte Carlo) or se = "bootstrap" may 
#>    be more accurate.

md_predictors <- mahalanobis_predictors(fit)
md_predictors
#> 
#> -- Mahalanobis Distance --
#> 
#>       md
#> 13 7.179
#> 45 6.707
#> 50 6.297
#> 33 5.479
#> 43 5.115
#> 25 4.909
#> 27 4.685
#> 20 4.378
#> 32 4.157
#> 34 3.432
#> 
#> Note:
#> - Only the first 10 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
#> - Cases sorted by Mahalanobis distance in decreasing order.
#> - Mahalanobis distance computed only on predictors.
```
