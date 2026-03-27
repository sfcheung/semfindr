# Case Influence Measures

Gets a
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
output and computes the changes in selected parameters and fit measures
for each case if included.

## Usage

``` r
influence_stat(
  rerun_out,
  fit_measures = c("chisq", "cfi", "rmsea", "tli"),
  baseline_model = NULL,
  parameters = NULL,
  mahalanobis = TRUE,
  keep_fit = TRUE
)
```

## Arguments

- rerun_out:

  The output from
  [`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md),
  or the output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers (e.g.,
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)).

- fit_measures:

  The argument `fit.measures` used in
  [lavaan::fitMeasures](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  Default is `c("chisq", "cfi", "rmsea", "tli")`.

- baseline_model:

  The argument `baseline.model` used in
  [lavaan::fitMeasures](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  Default is `NULL`.

- parameters:

  A character vector to specify the selected parameters. Each parameter
  is named as in `lavaan` syntax, e.g., `x ~ y` or `x ~~ y`, as appeared
  in the columns `lhs`, `op`, and `rhs` in the output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  Supports specifying an operator to select all parameters with this
  operators: `~`, `~~`, `=~`, and `~1`. This vector can contain both
  parameter names and operators. More details can be found in the help
  of
  [`pars_id()`](https://sfcheung.github.io/semfindr/reference/pars_id.md).
  If omitted or `NULL`, the default, changes on all free parameters will
  be computed.

- mahalanobis:

  If `TRUE`, it will call
  [`mahalanobis_rerun()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_rerun.md)
  to compute the Mahalanobis distance. Default is `TRUE`.

- keep_fit:

  If `TRUE`, it will keep the original `lavaan` output using the full
  sample as an attribute to the output. It can be used by other
  functions to extract necessary information. Default is `TRUE`.

## Value

An `influence_stat`-class object, which is a matrix with the number of
columns equals to the number of requested statistics, and the number of
rows equals to the number of cases. The row names are the case
identification values used in
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).
Please refer to the help pages of
[`est_change()`](https://sfcheung.github.io/semfindr/reference/est_change.md)
and
[`fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change.md)
(or
[`est_change_approx()`](https://sfcheung.github.io/semfindr/reference/est_change_approx.md)
and
[`fit_measures_change_approx()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change_approx.md)
for details. This object has a print method for printing user-friendly
output.

## Details

For each case, `influence_stat()` computes the differences in the
estimates of selected parameters and fit measures with and without this
case. Users can also request a measure of extremeness (only Mahalanobis
distance is available for now).

If `rerun_out` is the output of
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md),
it will use the leave-one-out approach. Measures are computed by
[`est_change()`](https://sfcheung.github.io/semfindr/reference/est_change.md)
(for DFTHETAS) and
[`fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change.md).

If `rerun_out` is the output of
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g.,
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) or
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)), it will use
the approximate approach. Measures are computed by
[`est_change_approx()`](https://sfcheung.github.io/semfindr/reference/est_change_approx.md)
(for approximate DFTHETAS) and
[`fit_measures_change_approx()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change_approx.md).

If Mahalanobis distance is requested, it is computed by
[`mahalanobis_rerun()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_rerun.md).

Please refer to the help pages of the above functions on the technical
details.

Supports both single-group and multiple-group models. (Support for
multiple-group models available in 0.1.4.8 and later version).

## References

Cheung, S. F., & Lai, M. H. C. (2026). `semfindr`: An R package for
identifying influential cases in structural equation modeling.
*Multivariate Behavioral Research*. Advance online publication.
doi:10.1080/00273171.2026.2634293

Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural
equation models: Cases and their influence. *Multivariate Behavioral
Research, 46*(2), 202-228. doi:10.1080/00273171.2011.561068

## See also

[`fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change.md),
[`est_change()`](https://sfcheung.github.io/semfindr/reference/est_change.md),
and
[`mahalanobis_rerun()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_rerun.md).

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

# --- Leave-One-Out Approach

# Fit the model n times. Each time with one case removed.
# For illustration, do this only for selected cases.
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.46 second(s).
#> Could be faster if run in parallel.
# Get all default influence stats
out <- influence_stat(fit_rerun)
head(out)
#>         chisq           cfi         rmsea           tli           a1
#> 1  0.15407944 -0.0019968390  0.0017700811 -0.0049920976  0.024466586
#> 2 -0.01944571  0.0011486763 -0.0010912308  0.0028716908  0.007153846
#> 3 -0.41673808  0.0082048797 -0.0074508538  0.0205121991 -0.038282397
#> 4 -0.15430823  0.0036670450 -0.0032789585  0.0091676124 -0.024048244
#> 5  0.09730667  0.0002954311  0.0008280336  0.0007385778  0.066686613
#> 6  0.11601736 -0.0010911517  0.0011378622 -0.0027278793  0.004007056
#>              a2            b      m1~~m1      dv~~dv         gcd        md
#> 1 -0.0300705396  0.051965997 -0.03663071  0.01717427 0.005891665 1.9107778
#> 2  0.0034230301 -0.013043400 -0.06744802 -0.05802199 0.008147128 0.4442464
#> 3 -0.0401051535 -0.029790144 -0.06335355 -0.04479763 0.009834826 3.7867385
#> 4 -0.0031358865  0.021674577 -0.05137193 -0.04379632 0.005610493 1.0653437
#> 5  0.0278462201  0.032782898  0.04979077 -0.06598323 0.013001467 1.9803351
#> 6  0.0009699846  0.009509592 -0.06910195 -0.05422999 0.007823146 0.2875484

# --- Approximate Approach

out_approx <- influence_stat(fit)
head(out_approx)
#>         chisq           cfi         rmsea          tli       m1~iv1
#> 1  0.15956516 -0.0020929857  0.0018614175 -0.005232464  0.024713396
#> 2 -0.01892880  0.0011399150 -0.0010827859  0.002849787  0.007254736
#> 3 -0.38907022  0.0077186573 -0.0070161267  0.019296643 -0.037982774
#> 4 -0.15078126  0.0036048257 -0.0032221332  0.009012064 -0.024353717
#> 5  0.09685352  0.0003104848  0.0008205378  0.000776212  0.067010210
#> 6  0.11602751 -0.0010913266  0.0011380304 -0.002728316  0.004065567
#>         m1~iv2        dv~m1      m1~~m1      dv~~dv  gcd_approx        md
#> 1 -0.030383580  0.052370026 -0.03784100  0.01630488 0.005850455 1.9107778
#> 2  0.003469647 -0.013223396 -0.06916983 -0.05953293 0.008312026 0.4442464
#> 3 -0.039820283 -0.030150418 -0.06518369 -0.04609708 0.009904544 3.7867385
#> 4 -0.003172373  0.021948305 -0.05277394 -0.04504791 0.005719499 1.0653437
#> 5  0.027951233  0.032981527  0.04860722 -0.06774242 0.012793226 1.9803351
#> 6  0.000983731  0.009640981 -0.07086163 -0.05565453 0.007984573 0.2875484
```
