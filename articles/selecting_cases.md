# Selecting Cases In lavaan_rerun

This vignette illustrates how to select cases when calling
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
from the package [semfindr](https://sfcheung.github.io/semfindr/)
(Cheung & Lai, 2026).

Instead of refitting the model *n* times, each time with each case
removed, it is possible to use some criteria to select cases to be
included in the analysis. This is useful when the time to fit a model is
long and/or the sample size is large. This can also be used together
with the approximate approach: Use the approximate approach to identify
potentially influential case and then compute the exact influence
statistics for these cases. See
[`vignette("casewise_scores", package = "semfindr")`](https://sfcheung.github.io/semfindr/articles/casewise_scores.md)
for further information on the the approximate approach.

The sample data set `pa_dat` will be used for illustration:

``` r
library(semfindr)
dat <- pa_dat
# Add case id
dat <- cbind(id = paste0("case", seq_len(nrow(dat))), dat)
head(dat)
#>      id          m1         dv        iv1         iv2
#> 1 case1  0.32067106  1.4587148  0.2055776 -0.42187811
#> 2 case2  0.15360231 -0.3809220  0.1853543  0.15229953
#> 3 case3  0.35136439 -0.4886773  0.9151424  1.16670950
#> 4 case4 -0.56529330 -0.9766142  0.2884440  0.04563409
#> 5 case5 -1.60657017 -1.0948066 -0.5756171 -0.18184854
#> 6 case6  0.03143301  0.5859886  0.1420111  0.06286986
```

The following model is fitted to the data set:

``` r
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
mod <-
"
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
"
fit <- sem(mod, dat)
```

## Row Numbers or Case IDs

Suppose, for some reasons, users want to refit a model only with
selected rows removed. For example, only rows 1, 4, 15, and 18 should be
selected. This can be done using the argument `to_rerun` of
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md):

``` r
rerun_out <- lavaan_rerun(fit,
                          to_rerun = c(1, 4, 15, 18))
#> The expected CPU time is 0.67 second(s).
#> Could be faster if run in parallel.
```

Only four reruns in the output:

``` r
rerun_out
#> === lavaan_rerun Output ===
#> Call:
#> lavaan_rerun(fit = fit, to_rerun = c(1, 4, 15, 18))
#> Number of reruns: 4
#> Number of reruns that converged (solution found): 4
#> Number of reruns that failed to converge (solution not found): 0
#> Number of reruns that passed post.check of lavaan: 4
#> Number of reruns that failed post.check of lavaan: 0
#> Number of reruns that both converged and passed post.check: 4
#> Number of reruns that either did not converge or failed post.check: 0
est_change(rerun_out)
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>    iv1~~iv2 m1~iv1 m1~iv2  dv~m1 m1~~m1 dv~~dv iv1~~iv1 iv2~~iv2   gcd
#> 18   -0.023 -0.273  0.035  0.101  0.260 -0.046    0.043   -0.067 0.163
#> 15    0.008  0.001  0.003 -0.009 -0.070 -0.061   -0.070   -0.058 0.017
#> 4     0.006 -0.024 -0.003  0.022 -0.051 -0.044   -0.056   -0.070 0.014
#> 1    -0.013  0.024 -0.030  0.052 -0.037  0.017   -0.061   -0.056 0.013
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
```

If user supplied case IDs are used, then the value of `to_rerun` should
be a vector of these case IDs:

``` r
rerun_out <- lavaan_rerun(fit,
                          case_id = dat$id,
                          to_rerun = c("case1",
                                       "case4",
                                       "case15",
                                       "case18"))
#> The expected CPU time is 0.67 second(s).
#> Could be faster if run in parallel.
```

Only four reruns in the output. User supplied case IDs are used in the
output:

``` r
rerun_out
#> === lavaan_rerun Output ===
#> Call:
#> lavaan_rerun(fit = fit, case_id = dat$id, to_rerun = c("case1", 
#>     "case4", "case15", "case18"))
#> Number of reruns: 4
#> Number of reruns that converged (solution found): 4
#> Number of reruns that failed to converge (solution not found): 0
#> Number of reruns that passed post.check of lavaan: 4
#> Number of reruns that failed post.check of lavaan: 0
#> Number of reruns that both converged and passed post.check: 4
#> Number of reruns that either did not converge or failed post.check: 0
est_change(rerun_out)
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        iv1~~iv2 m1~iv1 m1~iv2  dv~m1 m1~~m1 dv~~dv iv1~~iv1 iv2~~iv2   gcd
#> case18   -0.023 -0.273  0.035  0.101  0.260 -0.046    0.043   -0.067 0.163
#> case15    0.008  0.001  0.003 -0.009 -0.070 -0.061   -0.070   -0.058 0.017
#> case4     0.006 -0.024 -0.003  0.022 -0.051 -0.044   -0.056   -0.070 0.014
#> case1    -0.013  0.024 -0.030  0.052 -0.037  0.017   -0.061   -0.056 0.013
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
```

## Mahalanobis Distance on Residuals

Users can select cases using their rankings on the Mahalanobis distance
computed using the regression-based residuals. This is possible only for
models with observed variables only (i.e., path models). This is
analogous to selecting cases based on their residuals in a multiple
regression model. A path model can have more than one endogenous
variable. The residuals of a case on all endogenous variables will be
computed (as differences between observed scores and implied scores
computed by
[`implied_scores()`](https://sfcheung.github.io/semfindr/reference/implied_scores.md)),
and the Mahalanobis distance will be computed using these residuals.

This is done using the argument `resid_md_top`. Users specify the top
*x* cases on this distance to be selected for refitting a model.

``` r
rerun_out <- lavaan_rerun(fit,
                          case_id = dat$id,
                          resid_md_top = 5)
#> The expected CPU time is 0.83 second(s).
#> Could be faster if run in parallel.
```

Five cases were selected, as shown below:

``` r
rerun_out
#> === lavaan_rerun Output ===
#> Call:
#> lavaan_rerun(fit = fit, case_id = dat$id, resid_md_top = 5)
#> Number of reruns: 5
#> Number of reruns that converged (solution found): 5
#> Number of reruns that failed to converge (solution not found): 0
#> Number of reruns that passed post.check of lavaan: 5
#> Number of reruns that failed post.check of lavaan: 0
#> Number of reruns that both converged and passed post.check: 5
#> Number of reruns that either did not converge or failed post.check: 0
est_change(rerun_out)
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        iv1~~iv2 m1~iv1 m1~iv2  dv~m1 m1~~m1 dv~~dv iv1~~iv1 iv2~~iv2   gcd
#> case16   -0.019  0.052 -0.038 -0.237 -0.004  0.624   -0.049   -0.059 0.456
#> case43    0.236 -0.403 -0.263 -0.135  0.223  0.120    0.195    0.030 0.407
#> case65    0.133  0.152  0.191  0.363  0.076  0.161    0.000    0.046 0.241
#> case85   -0.066 -0.174  0.216 -0.119  0.335 -0.052   -0.031   -0.011 0.214
#> case61    0.004 -0.007 -0.021 -0.153  0.350 -0.031   -0.070   -0.070 0.157
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
```

Note that selecting cases by this method *can* miss some influential
cases. As in multiple regression, a case that is influential on the
results needs not be a case that is poorly predicted by the exogenous
variables. Therefore, this method should be used with caution.

## Mahalanobis Distance on All Variables

Users can select cases using their rankings on the Mahalanobis distance
computed using all observed variables. This is done using the argument
`md_top`. Users specify the top *x* cases on this distance to be
selected for refitting a model.

``` r
rerun_out <- lavaan_rerun(fit,
                          case_id = dat$id,
                          md_top = 5)
#> The expected CPU time is 0.83 second(s).
#> Could be faster if run in parallel.
```

Five cases were selected, as shown below:

``` r
rerun_out
#> === lavaan_rerun Output ===
#> Call:
#> lavaan_rerun(fit = fit, case_id = dat$id, md_top = 5)
#> Number of reruns: 5
#> Number of reruns that converged (solution found): 5
#> Number of reruns that failed to converge (solution not found): 0
#> Number of reruns that passed post.check of lavaan: 5
#> Number of reruns that failed post.check of lavaan: 0
#> Number of reruns that both converged and passed post.check: 5
#> Number of reruns that either did not converge or failed post.check: 0
est_change(rerun_out)
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        iv1~~iv2 m1~iv1 m1~iv2  dv~m1 m1~~m1 dv~~dv iv1~~iv1 iv2~~iv2   gcd
#> case99    0.163  0.002  0.009  0.187 -0.070  0.033   -0.054    0.686 0.559
#> case51   -0.130  0.421 -0.057  0.094  0.089 -0.044    0.525   -0.056 0.492
#> case16   -0.019  0.052 -0.038 -0.237 -0.004  0.624   -0.049   -0.059 0.456
#> case87   -0.419 -0.011  0.019  0.101 -0.070  0.000    0.096    0.457 0.413
#> case43    0.236 -0.403 -0.263 -0.135  0.223  0.120    0.195    0.030 0.407
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
```

Note that selecting cases by this method *can* miss some influential
cases (Pek & MacCallum, 2011). Unlike multiple regression, this distance
is not a measure of leverage. For a path model, this distance used
distances from the centroid on *all* observed variables, including
exogenous variables and endogenous variables. For a model with latent
factors, this distance is affected by both residuals and values
predicted by the latent factors. Therefore, this method should be used
with caution.

## Final Remarks

If feasible, it is recommended to refit a model once for each case, such
that the influential of all cases can be considered together. The
methods above are included when the processing time is slow and so only
selected cases are to be explored. For the final model(s),
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
using all cases are recommended, to serve as a final check on the
sensitivity of the results to individual cases.

## References

Cheung, S. F., & Lai, M. H. C. (2026). semfindr: An R package for
identifying influential cases in structural equation modeling.
*Multivariate Behavioral Research*.
<https://doi.org/10.1080/00273171.2026.2634293>

Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural
equation models: Cases and their influence. *Multivariate Behavioral
Research*, *46*(2), 202–228.
<https://doi.org/10.1080/00273171.2011.561068>
