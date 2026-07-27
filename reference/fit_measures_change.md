# Case Influence on Fit Measures

Gets a
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
output and computes the changes in selected fit measures if a case is
included.

## Usage

``` r
fit_measures_change(
  rerun_out,
  fit_measures = c("chisq", "cfi", "rmsea", "tli"),
  baseline_model = NULL
)
```

## Arguments

- rerun_out:

  The output from
  [`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).

- fit_measures:

  The argument `fit.measures` used in
  [lavaan::fitMeasures](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  Default is `c("chisq", "cfi", "rmsea", "tli")`.

- baseline_model:

  The argument `baseline.model` used in
  [lavaan::fitMeasures](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  Default is `NULL`.

## Value

An `fit_measures_change`-class object, which is matrix with the number
of columns equals to the number of requested fit measures, and the
number of rows equals to the number of cases. The row names are the case
identification values used in
[`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md).
A print method is available for user-friendly output.

## Details

For each case, `fit_measures_change()` computes the differences in
selected fit measures with and without this case:

(Fit measure with all case) - (Fit measure without this case).

If the value of a case is positive, including the case increases an
estimate.

If the value of a case is negative, including the case decreases an
estimate.

Note that an increase is an improvement in fit for goodness of fit
measures such as CFI and TLI, but a decrease is an improvement in fit
for badness of fit measures such as RMSEA and model chi-square. This is
a measure of the influence of a case on a fit measure if it is included.

If the analysis is not admissible or does not converge when a case is
deleted, `NA`s will be turned for the differences of this case.

Supports both single-group and multiple-group models. (Support for
multiple-group models available in 0.1.4.8 and later version).

## References

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
# Fit the model n times. Each time with one case removed.
# For illustration, do this only for four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.49 second(s).
#> Could be faster if run in parallel.
# Compute the changes in chisq if a case is included
# vs. if this case is removed.
# That is, case influence on model chi-squared.
out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
# Results excluding a case, for the first few cases
head(out)
#>         chisq
#> 1  0.15407944
#> 2 -0.01944571
#> 3 -0.41673808
#> 4 -0.15430823
#> 5  0.09730667
#> 6  0.11601736
# Chi-square will all cases included.
(chisq_all <- fitMeasures(fit, c("chisq")))
#> chisq 
#> 6.711 
# Chi-square with the first case removed
fit_01 <- lavaan::sem(mod, dat[-1, ])
(chisq_no_1 <- fitMeasures(fit_01, c("chisq")))
#> chisq 
#> 6.557 
# Difference
chisq_all - chisq_no_1
#> chisq 
#> 0.154 
# Compare to the result from the fit_measures_change
out[1, ]
#> [1] 0.1540794

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
out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
head(out)
#>         chisq
#> 1 -0.99210274
#> 2  0.90763725
#> 3 -0.25394609
#> 4 -0.03468346
#> 5 -0.63365168
#> 6  0.59956838
(chisq_all <- fitMeasures(fit, c("chisq")))
#>  chisq 
#> 12.027 
fit_01 <- lavaan::sem(mod, dat[-1, ])
(chisq_no_1 <- fitMeasures(fit_01, c("chisq")))
#>  chisq 
#> 13.019 
chisq_all - chisq_no_1
#>  chisq 
#> -0.992 
out[1, ]
#> [1] -0.9921027

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

fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
#> The expected CPU time is 0.68 second(s).
#> Could be faster if run in parallel.
out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
head(out)
#>        chisq
#> 1 -0.3385932
#> 2 -0.6367473
#> 3  0.5199304
#> 4  1.2366084
#> 5 -0.5428558
#> 6 -0.2635458
(chisq_all <- fitMeasures(fit, c("chisq")))
#>  chisq 
#> 41.768 
fit_01 <- lavaan::sem(mod, dat[-1, ])
(chisq_no_1 <- fitMeasures(fit_01, c("chisq")))
#>  chisq 
#> 42.107 
chisq_all - chisq_no_1
#>  chisq 
#> -0.339 
out[1, ]
#> [1] -0.3385932
```
