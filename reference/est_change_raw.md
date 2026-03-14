# Case Influence on Parameter Estimates (DFTHETA)

Gets a
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
output and computes the changes in selected parameters for each case if
included.

## Usage

``` r
est_change_raw(
  rerun_out,
  parameters = NULL,
  standardized = FALSE,
  user_defined_label_full = FALSE
)
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
  Supports specifying an operator to select all parameters with these
  operators: `~`, `~~`, `=~`, and `~1`. This vector can contain both
  parameter names and operators. More details can be found in the help
  of
  [`pars_id()`](https://sfcheung.github.io/semfindr/reference/pars_id.md).
  If omitted or `NULL`, the default, changes on all free parameters will
  be computed.

- standardized:

  If `TRUE`, the changes in the full standardized solution (*DFZTHETA*s)
  are returned (`type` = `std.all` in
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)).
  Otherwise, the changes in the unstandardized solution (*DFTHETA*s) are
  returned. Default is `FALSE`.

- user_defined_label_full:

  Logical. If `TRUE`, use the full labels for user-defined parameters
  (parameters created by `:=`), which include the definition. If
  `FALSE`, then only the label on the right-hand side of `:=` will be
  used. Default is `FALSE`. In previous version, the full labels were
  used. Set to `TRUE` if backward compatibility is needed.

## Value

An `est_change`-class object, which is matrix with the number of columns
equals to the number of requested parameters, and the number of rows
equals to the number of cases. The row names are the case identification
values used in
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).
The elements are the raw differences. A print method is available for
user-friendly output.

## Details

For each case, `est_change_raw()` computes the differences in the
estimates of selected parameters with and without this case:

(Estimate with all case) - (Estimate without this case).

The change is the raw change (*DFTHETA*, Cheung & Lai, 2026, Pek &
MacCallum, 2011), either for the standardized or unstandardized
solution. The change is *not* divided by standard error. This is a
measure of the influence of a case on the parameter estimates if it is
included.

If the value of a case is positive, including the case increases an
estimate.

If the value of a case is negative, including the case decreases an
estimate.

If the analysis is not admissible or did not converge when a case is
deleted, `NA`s will be returned for this case on the differences.

The corresponding changes in the standardized solution (e.g., a change
in a correlation) is called *DFZTHETA*, *Z* for standardized solution.

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

# Fit the model n times. Each time with one case is removed.
# For illustration, do this only for four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(3, 5, 7, 8))
#> The expected CPU time is 0.2 second(s).
#> Could be faster if run in parallel.

# === DFTHETA ===

# Compute the changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, case influence on parameter estimates
out <- est_change_raw(fit_rerun)
# Results excluding a case
out
#> 
#> -- Case Influence on Parameter Estimates --
#> 
#>   id m1~iv1 id m1~iv2 id  dv~m1 id m1~~m1 id dv~~dv id    a1b id    a2b
#> 1  7 -0.013  7  0.007  7  0.007  3 -0.008  8 -0.013  7 -0.005  7  0.007
#> 2  5  0.007  8  0.007  5  0.004  8 -0.007  5 -0.013  5  0.004  8  0.005
#> 3  8  0.006  3 -0.004  3 -0.003  5  0.006  3 -0.008  8  0.004  3 -0.004
#> 4  3 -0.004  5  0.003  8  0.003  7  0.000  7 -0.008  3 -0.003  5  0.003
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.
# Note that these are the differences in parameter estimates.

# The parameter estimates from all cases
(coef_all <- coef(fit))
#>     a1     a2      b m1~~m1 dv~~dv 
#>  0.215  0.522  0.517  0.903  1.321 
# The parameter estimates from manually deleting the third case
fit_no_3 <- lavaan::sem(mod, dat[-3, ])
(coef_no_3 <- coef(fit_no_3))
#>     a1     a2      b m1~~m1 dv~~dv 
#>  0.219  0.526  0.520  0.911  1.330 
# The differences
coef_all - coef_no_3
#>     a1     a2      b m1~~m1 dv~~dv 
#> -0.004 -0.004 -0.003 -0.008 -0.008 
# The first row of `est_change_raw(fit_rerun)`
round(out[1, ], 3)
#> m1~iv1 m1~iv2  dv~m1 m1~~m1 dv~~dv    a1b    a2b 
#> -0.004 -0.004 -0.003 -0.008 -0.008 -0.003 -0.004 

# Compute only the changes of the paths from iv1 and iv2 to m1
out2 <- est_change_raw(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
# Results excluding a case
out2
#> 
#> -- Case Influence on Parameter Estimates --
#> 
#>   id m1~iv1 id m1~iv2
#> 1  7 -0.013  7  0.007
#> 2  5  0.007  8  0.007
#> 3  8  0.006  3 -0.004
#> 4  3 -0.004  5  0.003
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.
# Note that only the changes in the selected paths are included.

# === DFZTHETA (Z for standardized solution) ===

# Use standardized = TRUE to compare the differences in standardized solution
out2_std <- est_change_raw(fit_rerun,
                           c("m1 ~ iv1", "m1 ~ iv2"),
                           standardized = TRUE)
out2_std
#> 
#> -- Case Influence on Standardized Parameter Estimates --
#> 
#>   id m1~iv1 id m1~iv2
#> 1  7 -0.010  8  0.006
#> 2  8  0.005  7  0.006
#> 3  5  0.005  5 -0.001
#> 4  3 -0.002  3 -0.001
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.
(est_std_all <- parameterEstimates(fit,
                 standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
#>   lhs op rhs std.all
#> 1  m1  ~ iv1   0.178
#> 2  m1  ~ iv2   0.459
(est_std_no_1 <- parameterEstimates(fit_no_3,
                 standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
#>   lhs op rhs std.all
#> 1  m1  ~ iv1    0.18
#> 2  m1  ~ iv2    0.46
# The differences
est_std_all$std.all - est_std_no_1$std.all
#> [1] -0.0023823159 -0.0006520026
# The first row of `out2_std`
out2_std[1, ]
#>        m1~iv1        m1~iv2 
#> -0.0023823159 -0.0006520026 

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
#> The expected CPU time is 0.23 second(s).
#> Could be faster if run in parallel.
# Compute the changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, case influence on parameter estimates.
# === DFTHETA ===
# For free loadings only
out <- est_change_raw(fit_rerun, parameters = "=~")
out
#> 
#> -- Case Influence on Parameter Estimates --
#> 
#>   id f1=~x2 id f1=~x3 id f2=~x5 id f2=~x6
#> 1  3 -0.307  3 -0.172  3 -1.016  2 -0.033
#> 2  2  0.097  2  0.121  2  0.121  3 -0.020
#> 3  7 -0.005  5 -0.032  7 -0.025  5 -0.010
#> 4  5 -0.004  7 -0.003  5  0.005  7 -0.004
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.
# === DFZTHETA ===
# For standardized loadings only
out_std <- est_change_raw(fit_rerun, parameters = "=~",
                          standardized = TRUE)
out_std
#> 
#> -- Case Influence on Standardized Parameter Estimates --
#> 
#>   id f1=~x1 id f1=~x2 id f1=~x3 id f2=~x4 id f2=~x5 id f2=~x6
#> 1  3  0.104  3 -0.059  2  0.029  3  0.095  3 -0.104  3  0.096
#> 2  2 -0.038  2  0.032  3  0.014  2 -0.011  2  0.023  2 -0.032
#> 3  5  0.010  7 -0.005  5 -0.009  7  0.004  5  0.002  5 -0.005
#> 4  7  0.000  5 -0.001  7 -0.001  5 -0.003  7 -0.001  7  0.003
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.

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
#> The expected CPU time is 0.26 second(s).
#> Could be faster if run in parallel.
# Compute the changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, case influence on parameter estimates.
# === DFTHETA ===
# For structural paths only
out <- est_change_raw(fit_rerun, parameters = "~")
out
#> 
#> -- Case Influence on Parameter Estimates --
#> 
#>   id  f2~f1 id  f3~f2
#> 1  3 -0.030  7  0.007
#> 2  5 -0.016  3 -0.006
#> 3  7 -0.011  2 -0.006
#> 4  2  0.010  5 -0.001
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.
# === DFZTHETA ===
# For standardized paths only
out_std <- est_change_raw(fit_rerun, parameters = "~",
                          standardized = TRUE)
out_std
#> 
#> -- Case Influence on Standardized Parameter Estimates --
#> 
#>   id  f2~f1 id  f3~f2
#> 1  3 -0.013  7  0.004
#> 2  7  0.012  3  0.004
#> 3  2  0.003  5 -0.002
#> 4  5  0.000  2 -0.001
#> 
#> Note:
#> - Changes are raw changes if a case is included.
#> - All stored cases are displayed.
#> - Cases sorted by the absolute changes for each variable.

```
