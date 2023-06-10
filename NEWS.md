# semfindr 0.1.4.6

- Fixed the pkgdown site. (0.1.4.1)
- Check whether a model has at least one equality constraint
  and the selected parameters are involved in the constraint(s).
  If yes, *gCD* will not be computed. (0.1.4.2)
- `approx_check()` will check if the model has at least one
  equality constraint. (0.1.4.2)
- Added a "Limitations" section to the vignette of the
  approximate approach. (0.1.4.2)
- Added a print method for the output of `est_change()`,
  `est_change_raw()`, `est_change_approx()`, and
  `est_change_raw_approx()`. (0.1.4.3)
- Added a print method for the output of `fit_measures_change()`
  and `fit_measures_change_approx()`. (0.1.4.4)
- Added a print method for the output of `mahalanobis_rerun()`.
  (0.1.4.5)
- Updated the print method of `md_semfindr` to
  support the output of `mahalanobis_predictors()`.
  (0.1.4.6)

# semfindr 0.1.4

- Added DOI of Pek and MacCallum in the DESCRIPTION. (0.1.1.1)
- Added `approx_check()` to check whether the input object
  is supported by the approximate method. (0.1.1.2)
- Added `est_change_plot()` and `est_change_gcd_plot()`,
  diagnostic plots for casewise influence on
  parameter estimates. (0.1.1.3)
- Diagnostic plot functions revised to allow users to
  fully control elements drawn. (0.1.1.4)
- Fixed typos and grammatical mistakes in help pages and
  vignettes. (0.1.1.5)
- Fixed an invalid URI in a vignette (casewise_scores). (0.1.3)
- Added the documentation for the return value of `pars_id_to_lorg()`. (0.1.4)

# semfindr 0.1.1

- First public release.
- Added `skip_all_checks` to `lavaan_rerun()`, allowing users
  to experiment `lavaan_rerun()` and other functions on
  models not officially supported.
- Revised `est_change()` and `est_change_raw()` to support
  the use of operators (e.g., `~`, `=~`) to select parameters.
- Added badges and R CMD Check Action.
- Updated `est_change()`, `est_change_raw()` and
  `est_change_approx()` to support models with labelled
  parameters. (0.1.0.9005)
- Added `pars_id()` and `pars_id_to_lorg()` for converting
  parameter specification to identification numbers (
  positions in the vector of coefficients or row numbers
  in the parameter tables). (0.1.0.9006)
- Updated `est_change_*` functions to use `pars_id()`
  and `pars_id_to_lorg()`. (0.1.0.9007)
- Modified `lavaan_rerun()` to use `lavaan::lavaan()`
  instead of `update()` as the default way to rerun. (0.1.0.9008).
- Updated some of the tests. (0.1.0.9009)
- Added more examples. (0.1.0.9010)
- Updated documentation (e.g., README and DESCRIPTION). (0.1.0.9011)
- Updated `influence_stat()` and the plot functions to support
  the approximate approach. (0.1.0.9012)
- Updated documentation.

# semfindr 0.1.0

- Added more vignettes.

- `lavaan_rerun()` can accept an output with inadmissible
  estimates. Disabled by default. Can be enabled by
  setting `allow_inadmissible` to `TRUE`.

# semfindr 0.0.4

- Added a print method for the `lavaan_rerun()` class.

- Added `mahalanobis_predictors()` to compute the
Mahalanobis distance using only the observed predictors.

- Both `mahalanobis_predictors()` and
 `mahalanobis_rerun()` support datasets with missing data.

- `lavaan_rerun()` can specify cases to
exclude and rerun by specifying the case IDs or
selecting cases based on Mahalanobis distance on
all observed variables or on residuals of observed
variables in a path model.

# semfindr 0.0.3

- Used `lavaan::update()` in lavaan_rerun. This is more
  reliable than recreating the call.

- Added `implied_scores()`. It supports only single-group
  path analysis models for now.

# semfindr 0.0.2

- First internal testing release.
