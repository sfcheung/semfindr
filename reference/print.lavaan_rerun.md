# Print Method for 'lavaan_rerun'

Prints the results of
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).

## Usage

``` r
# S3 method for class 'lavaan_rerun'
print(x, ...)
```

## Arguments

- x:

  The output of
  [`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).

- ...:

  Other arguments. They will be ignored.

## Value

`x` is returned invisibly. Called for its side effect.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
dat <- pa_dat
# For illustration only, select only the first 50 cases
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
#> lavaan 0.6-21 ended normally after 1 iteration
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
# Fit the model n times. Each time with one case removed.
fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#> The expected CPU time is 2.1 second(s).
#> Could be faster if run in parallel.
fit_rerun
#> === lavaan_rerun Output ===
#> Call:
#> lavaan_rerun(fit = fit, parallel = FALSE)
#> Number of reruns: 50
#> Number of reruns that converged (solution found): 50
#> Number of reruns that failed to converge (solution not found): 0
#> Number of reruns that passed post.check of lavaan: 50
#> Number of reruns that failed post.check of lavaan: 0
#> Number of reruns that both converged and passed post.check: 50
#> Number of reruns that either did not converge or failed post.check: 0
```
