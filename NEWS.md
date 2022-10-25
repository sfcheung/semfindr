# semfindr 0.1.0.9005

(Work-In-Progress. Cumulated updates to be included in 0.1.1)

- Added `skip_all_checks` to `lavaan_rerun()`, allowing users
  to experiment `lavaan_rerun()` and other functions on
  models not officially supported.
- Revised `est_change()` and `est_change_raw()` to support
  the use of operators (e.g., `~`, `=~`) to select parameters.
- Added badges and R CMD Check Action.
- Updated `est_change()`, `est_change_raw()` and
  `est_change_approx()` to support models with labelled
  parameters. (0.1.0.9005)

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

- Use `lavaan::update()` in lavaan_rerun. This is more
  reliable than recreating the call.

- Add `implied_scores()`. It supports only single-group
  path analysis models for now.

# semfindr 0.0.2

- First internal testing release.
