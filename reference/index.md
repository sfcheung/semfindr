# Package index

## Main Functions

- [`lavaan_rerun()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun.md)
  : Rerun a 'lavaan' Analysis Using the Leaving-One-Out Approach
- [`influence_stat()`](https://sfcheung.github.io/semfindr/reference/influence_stat.md)
  : Case Influence Measures

## Parameter Estimates

Case influence on parameter estimates

- [`est_change()`](https://sfcheung.github.io/semfindr/reference/est_change.md)
  : Standardized Case Influence on Parameter Estimates (DFTHETAS)
- [`est_change_raw()`](https://sfcheung.github.io/semfindr/reference/est_change_raw.md)
  : Case Influence on Parameter Estimates (DFTHETA)
- [`est_change_approx()`](https://sfcheung.github.io/semfindr/reference/est_change_approx.md)
  : Standardized Case Influence on Parameter Estimates (Approximate
  DFTHETAS)
- [`est_change_raw_approx()`](https://sfcheung.github.io/semfindr/reference/est_change_raw_approx.md)
  : Case Influence on Parameter Estimates (Approximate DFTHETA)

## Fit Measures

Case influence of fit measures

- [`fit_measures_change()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change.md)
  : Case Influence on Fit Measures
- [`fit_measures_change_approx()`](https://sfcheung.github.io/semfindr/reference/fit_measures_change_approx.md)
  : Case Influence on Fit Measures (Approximate)

## User Statistics

Case influence on any statistics computed by a user functions

- [`user_change_raw()`](https://sfcheung.github.io/semfindr/reference/user_change_raw.md)
  : Case Influence on User-Defined Statistics

## Extremeness Measures

Identifying outliers (which may not be influential)

- [`mahalanobis_predictors()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_predictors.md)
  : Mahalanobis Distance On Observed Predictors
- [`mahalanobis_rerun()`](https://sfcheung.github.io/semfindr/reference/mahalanobis_rerun.md)
  : Mahalanobis Distance on All Observed Variables

## Diagnostic Plots

- [`gcd_plot()`](https://sfcheung.github.io/semfindr/reference/influence_plot.md)
  [`md_plot()`](https://sfcheung.github.io/semfindr/reference/influence_plot.md)
  [`gcd_gof_plot()`](https://sfcheung.github.io/semfindr/reference/influence_plot.md)
  [`gcd_gof_md_plot()`](https://sfcheung.github.io/semfindr/reference/influence_plot.md)
  : Plots of Influence Measures
- [`est_change_plot()`](https://sfcheung.github.io/semfindr/reference/est_change_plot.md)
  [`est_change_gcd_plot()`](https://sfcheung.github.io/semfindr/reference/est_change_plot.md)
  : Plots of Case Influence on Parameter Estimates
- [`index_plot()`](https://sfcheung.github.io/semfindr/reference/index_plot.md)
  : Index Plot of an Arbitrary Statistic

## Print Methods

- [`print(`*`<lavaan_rerun>`*`)`](https://sfcheung.github.io/semfindr/reference/print.lavaan_rerun.md)
  : Print Method for 'lavaan_rerun'
- [`print(`*`<est_change>`*`)`](https://sfcheung.github.io/semfindr/reference/print.est_change.md)
  : Print an 'est_change' Class Object
- [`print(`*`<fit_measures_change>`*`)`](https://sfcheung.github.io/semfindr/reference/print.fit_measures_change.md)
  : Print a 'fit_measures_change' Class Object
- [`print(`*`<md_semfindr>`*`)`](https://sfcheung.github.io/semfindr/reference/print.md_semfindr.md)
  : Print a 'md_semfindr' Class Object
- [`print(`*`<influence_stat>`*`)`](https://sfcheung.github.io/semfindr/reference/print.influence_stat.md)
  : Print an 'influence_stat' Class Object

## Selecting Parameters

- [`pars_id()`](https://sfcheung.github.io/semfindr/reference/pars_id.md)
  : Convert Parameter Syntax to Position or Row Numbers in the Parameter
  Vector or Table
- [`pars_id_to_lorg()`](https://sfcheung.github.io/semfindr/reference/pars_id_to_lorg.md)
  : Ids to "lhs-op-rhs-(group)"

## Others

- [`lavaan_rerun_check()`](https://sfcheung.github.io/semfindr/reference/lavaan_rerun_check.md)
  : Compatibility Check for 'lavaan_rerun'
- [`approx_check()`](https://sfcheung.github.io/semfindr/reference/approx_check.md)
  : Compatibility Check for the '\_approx' Functions
- [`implied_scores()`](https://sfcheung.github.io/semfindr/reference/implied_scores.md)
  : Implied Scores of Observed Outcome Variables

## Datasets

- [`cfa_dat`](https://sfcheung.github.io/semfindr/reference/cfa_dat.md)
  : Sample Data: A CFA Model
- [`cfa_dat2`](https://sfcheung.github.io/semfindr/reference/cfa_dat2.md)
  : Sample Data: A CFA Model with an Influential Case
- [`cfa_dat_heywood`](https://sfcheung.github.io/semfindr/reference/cfa_dat_heywood.md)
  : Sample Data: A CFA Model with a Heywood Case
- [`cfa_dat_mg`](https://sfcheung.github.io/semfindr/reference/cfa_dat_mg.md)
  : Sample Data: A Multiple-Group CFA Model with an Influential Case
- [`pa_dat`](https://sfcheung.github.io/semfindr/reference/pa_dat.md) :
  Sample Data: A Path Model
- [`pa_dat2`](https://sfcheung.github.io/semfindr/reference/pa_dat2.md)
  : Sample Data: A Path Model with an Influential Case
- [`sem_dat`](https://sfcheung.github.io/semfindr/reference/sem_dat.md)
  : Sample Data: A Latent Variable Structural Model
- [`sem_dat2`](https://sfcheung.github.io/semfindr/reference/sem_dat2.md)
  : Sample Data: A Latent Variable Structural Model With an Influential
  Case
