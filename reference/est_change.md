# Standardized Case Influence on Parameter Estimates (DFTHETAS)

Gets a
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
output and computes the standardized changes in selected parameters for
each case if included.

## Usage

``` r
est_change(rerun_out, parameters = NULL)
```

## Arguments

- rerun_out:

  The output from
  [`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).

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

## Value

An `est_change`-class object, which is matrix with the number of columns
equals to the number of requested parameters plus one, the last column
being the generalized Cook's distance. The number of rows equal to the
number of cases. The row names are the case identification values used
in
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).
The elements are the standardized differences (*DFTHETAS*s). Please see
Pek and MacCallum (2011), Equation 7. A print method is available for
user-friendly output.

## Details

For each case, `est_change()` computes the differences in the estimates
of selected parameters with and without this case:

(Estimate with all case) - (Estimate without this case).

The differences are standardized by dividing the raw differences by
their standard errors (Pek & MacCallum, 2011). This measure, called
*DFTHETAS* to differentiate it from *DFZTHETA*
([`est_change_raw()`](https://sfcheung.github.io/semfindr/reference/est_change_raw.md)
with `standardized = TRUE`, see Cheung & Lai, 2026), is a measure of the
standardized influence of a case on the parameter estimates if it is
included.

If the value of a case is positive, including the case increases an
estimate.

If the value of a case is negative, including the case decreases an
estimate.

If the analysis is not admissible or does not converge when a case is
deleted, `NA`s will be turned for this case on the differences.

Unlike
[`est_change_raw()`](https://sfcheung.github.io/semfindr/reference/est_change_raw.md),
`est_change()` does not support computing the standardized changes of
standardized estimates.

It will also compute generalized Cook's distance (*gCD*), proposed by
Pek and MacCallum (2011) for structural equation modeling. Only the
parameters selected (all free parameters, by default) will be used in
computing *gCD*.

Since version 0.1.4.8, if (a) a model has one or more equality
constraints, and (b) some selected parameters are linearly dependent or
constrained to be equal due to the constraint(s), *gCD* will be computed
by removing parameters such that the remaining parameters are not
linearly dependent nor constrained to be equal. (Support for equality
constraints and linearly dependent parameters available in 0.1.4.8 and
later version).

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

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.

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
# Fit the model several times. Each time with one case removed.
# For illustration, do this only for four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(2, 4, 7, 9))
#> The expected CPU time is 0.22 second(s).
#> Could be faster if run in parallel.
# Compute the standardized changes in parameter estimates
# if a case is included vs. if this case is excluded.
# That is, case influence on parameter estimates, standardized.
out <- est_change(fit_rerun)
# Case influence:
out
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>       a1     a2      b m1~~m1 dv~~dv   gcd
#> 9 -0.048 -0.025 -0.083 -0.033  0.283 0.091
#> 7 -0.119  0.073  0.065 -0.002 -0.040 0.026
#> 2  0.007  0.003 -0.013 -0.067 -0.058 0.008
#> 4 -0.024 -0.003  0.022 -0.051 -0.044 0.006
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
# Note that these are the differences divided by the standard errors
# The rightmost column, `gcd`, contains the
# generalized Cook's distances (Pek & MacCallum, 2011).
out[, "gcd", drop = FALSE]
#>           gcd
#> 2 0.008147128
#> 4 0.005610493
#> 7 0.025740465
#> 9 0.090844702

# Compute the changes for the paths from iv1 and iv2 to m1
out2 <- est_change(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
# Case influence:
out2
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>       a1     a2   gcd
#> 7 -0.119  0.073 0.020
#> 9 -0.048 -0.025 0.003
#> 4 -0.024 -0.003 0.001
#> 2  0.007  0.003 0.000
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
# Note that only the changes in the selected parameters are included.
# The generalized Cook's distance is computed only from the selected
# parameter estimates.

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

# Examine four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(2, 3, 5, 7))
#> The expected CPU time is 0.38 second(s).
#> Could be faster if run in parallel.
# Compute the standardized changes in parameter estimates
# if a case is included vs. if a case is excluded.
# That is, case influence on parameter estimates, standardized.
# For free loadings only
out <- est_change(fit_rerun, parameters = "=~")
out
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>   f1=~x2 f1=~x3 f2=~x5 f2=~x6   gcd
#> 3 -0.916 -0.444 -0.514 -0.043 1.132
#> 2  0.463  0.446  0.155 -0.087 0.338
#> 5 -0.019 -0.106  0.006 -0.026 0.014
#> 7 -0.021 -0.011 -0.029 -0.011 0.001
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.

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

# Examine four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(2, 3, 5, 7))
#> The expected CPU time is 0.53 second(s).
#> Could be faster if run in parallel.
# Compute the changes in parameter estimates if a case is included
# vs. if a case is excluded.
# That is, standardized case influence on parameter estimates.
# For structural paths only
out <- est_change(fit_rerun, parameters = "~")
out
#> 
#> -- Standardized Case Influence on Parameter Estimates --
#> 
#>        a      b   gcd
#> 3 -0.123 -0.096 0.027
#> 7 -0.048  0.110 0.014
#> 2  0.044 -0.092 0.010
#> 5 -0.065 -0.018 0.005
#> 
#> Note:
#> - Changes are standardized raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by generalized Cook's distance.
```
