# Rerun a 'lavaan' Analysis Using the Leaving-One-Out Approach

Reruns a `lavaan` analysis several times, each time with one case
removed.

## Usage

``` r
lavaan_rerun(
  fit,
  case_id = NULL,
  to_rerun,
  md_top,
  resid_md_top,
  allow_inadmissible = FALSE,
  skip_all_checks = FALSE,
  parallel = FALSE,
  ncores = NULL,
  makeCluster_args = list(spec = getOption("cl.cores", 2)),
  progress = TRUE,
  rerun_method = c("lavaan", "update")
)
```

## Arguments

- fit:

  The output from
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers (e.g.,
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)).

- case_id:

  If it is a character vector of length equals to the number of cases
  (the number of rows in the data in `fit`), then it is the vector of
  case identification values. If it is `NULL`, the default, then
  `case.idx` used by `lavaan` functions will be used as case
  identification values. The case identification values will be used to
  name the list of *n* output.

- to_rerun:

  The cases to be processed. If `case_id` is specified, this should be a
  subset of `case_id`. If `case_id` is not specified, then this should
  be a vector of integers indicating the rows to te processed, as
  appeared in the data in `fit`. `to_rerun` cannot be used together with
  `md_top` or `resid_md_top.`

- md_top:

  The number of cases to be processed based on the Mahalanobis distance
  computed on all observed variables used in the model. The cases will
  be ranked from the largest to the smallest distance, and the top
  `md_top` case(s) will be processed. `md_top` cannot be used together
  with `to_rerun` or `resid_md_top.`

- resid_md_top:

  The number of cases to be processed based on the Mahalanobis distance
  computed from the residuals of outcome variables. The cases will be
  ranked from the largest to the smallest distance, and the top
  `resid_md_top` case(s) will be processed. `resid_md_top` cannot be
  used together with `to_rerun` or `md_top.`

- allow_inadmissible:

  If `TRUE`, accepts a fit object with inadmissible results (i.e.,
  `post.check` from
  [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  is `FALSE`). Default is `FALSE`.

- skip_all_checks:

  If `TRUE`, skips all checks and allow users to run this function on
  any object of `lavaan` class. For users to experiment this and other
  functions on models not officially supported. Default is `FALSE`.

- parallel:

  Whether parallel will be used. If `TRUE`, will use functions in the
  `parallel` package to rerun the analysis. Currently, only support
  `"snow"` type clusters using local CPU cores. Default is `FALSE`.

- ncores:

  The number of CPU cores to use if parallel processing is requested.
  Default is `NULL`, and the number of cores is determine by
  `makeCluster_args`. If set to an integer, this number will override
  the setting (`spec`) in `makeCluster_args`.

- makeCluster_args:

  A named list of arguments to be passed to
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  Default is `list(spec = getOption("cl.cores", 2)))`. If only the
  number of cores need to be specified, use `list(spec = x)`, where `x`
  is the number of cores to use. Alternatively, set `ncores` and its
  value will be used in `spec`.

- progress:

  If `TRUE`, the default, progress will be displayed on screen.

- rerun_method:

  How fit will be rerun. Default is `"lavaan"`. An alternative method is
  `"update"`. For internal use. If `"lavaan"` returns an error, try
  setting this argument to `"update"`.

## Value

A `lavaan_rerun`-class object, which is a list with the following
elements:

- `rerun`: The *n* `lavaan` output objects.

- `fit`: The original output from `lavaan`.

- `post_check`: A list of length equals to *n*. Each analysis was
  checked by
  [lavaan::lavTech](https://rdrr.io/pkg/lavaan/man/lavInspect.html)`(x, "post.check")`,
  `x` being the `lavaan` results. The results of this test are stored in
  this list. If the value is `TRUE`, the estimation converged and the
  solution is admissible. If not `TRUE`, it is a warning message issued
  by
  [`lavaan::lavTech()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html).

- `converged`: A vector of length equals to *n*. Each analysis was
  checked by
  [lavaan::lavTech](https://rdrr.io/pkg/lavaan/man/lavInspect.html)`(x, "converged")`,
  `x` being the `lavaan` results. The results of this test are stored in
  this vector. If the value is `TRUE`, the estimation converged. If not
  `TRUE`, then the estimation failed to converge if the corresponding
  case is excluded.

- `call`: The call to `lavaan_rerun()`.

- `selected`: A numeric vector of the row numbers of cases selected in
  the analysis. Its length should be equal to the length of `rerun`.

## Details

`lavaan_rerun()` gets an
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) output
and reruns the analysis *n0* times, using the same arguments and options
in the output, *n0* equals to the number of cases selected, by default
all cases in the analysis. In each run, one case will be removed.

Optionally, users can rerun the analysis with only selected cases
removed. These cases can be specified by case IDs, by Mahalanobis
distance computed from all variables used in the model, or by
Mahalanobis distance computed from the residuals (observed score -
implied scores) of observed outcome variables. See the help on the
arguments `to_rerun`, `md_top`, and `resid_md_top`.

It is not recommended to use Mahalanobis distance computed from all
variables, especially for models with observed variables as predictors
(Pek & MacCallum, 2011). Cases that are extreme on predictors may not be
influential on the parameter estimates. Nevertheless, this distance is
reported in some SEM programs and so this option is provided.

Mahalanobis distance based on residuals are supported for models with no
latent factors. The implied scores are computed by
[`implied_scores()`](https://sfcheung.github.io/semfindr/reference/implied_scores.md).

If the sample size is large, it is recommended to use parallel
processing. However, it is possible that parallel processing will fail.
If this is the case, try to use serial processing, by simply removing
the argument `parallel` or set it to `FALSE`.

Many other functions in
[semfindr](https://sfcheung.github.io/semfindr/reference/semfindr-package.md)
use the output from `lavaan_rerun()`. Instead of running the *n*
analyses every time, do this step once and then users can compute
whatever influence statistics they want quickly.

If the analysis took a few minutes to run due to the large number of
cases or the long processing time in fitting the model, it is
recommended to save the output to an external file (e.g., by
[`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html)).

Supports both single-group and multiple-group models. (Support for
multiple-group models available in 0.1.4.8 and later version).

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
#> The expected CPU time is 2.48 second(s).
#> Could be faster if run in parallel.

# Print the output for a brief description of the runs
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

# Results excluding the first case
fitMeasures(fit_rerun$rerun[[1]], c("chisq", "cfi", "tli", "rmsea"))
#> chisq   cfi   tli rmsea 
#> 1.403 1.000 1.143 0.000 
# Results by manually excluding the first case
fit_01 <- lavaan::sem(mod, dat[-1, ])
fitMeasures(fit_01, c("chisq", "cfi", "tli", "rmsea"))
#> chisq   cfi   tli rmsea 
#> 1.403 1.000 1.143 0.000 
```
