# semfindr 0.1.8

## Others

- Updated the two tests for nonconvergence.
  Will check against results generated
  by directly calling `lavaan` functions.
  (0.1.6.1)

- To be ready for newer versions of
  `lavaan`. (0.1.7 - 0.1.8)

## Bug Fixes

- Fixed a bug with listwise deletion
  in `lavaan_rerun()`. (0.1.6.2)

# semfindr 0.1.6

## New Features

- Added `index_plot()` for generating
  an index plot for an arbitrary
  statistic. (0.1.5.5)

- Added `user_change_raw()` for
  computing case influence on user
  statistics. (0.1.5.8)

## Others

- Fixed typos in the article on
  multiple-group models. (0.1.5.1)
- Fixed `pars_id()`. Users can now use
  the internal `lavaan` group labels
  (e.g., `".g2"`, `".g3"`) to denote
  parameters in a group. This method
  is not recommended (noted in the
  help page) but is included as an
  option. (0.1.5.2)
- Fixed an error at CRAN check due to
  suggested packages not installed. (0.1.5.3)
- Use `lavaan::lavCor()` to estimate
  means and correlations for Mahalanobis
  distance when missing data is present.
  The package `norm2` is no longer
  needed nor suggested. (0.1.5.4)
- Made plot functions to work with NAs.
  (0.1.5.6)
- Updated `README.md`. (0.1.5.7)
- Finalized 0.1.6. (0.1.6)

# semfindr 0.1.5

## New Features

- Added a print method for the output of `est_change()`,
  `est_change_raw()`, `est_change_approx()`, and
  `est_change_raw_approx()`. (0.1.4.3, 0.1.4.16, 0.1.4.18,
  0.1.4.19, 0.1.4.20)
- Added a print method for the output of `fit_measures_change()`
  and `fit_measures_change_approx()`. (0.1.4.4, 0.1.4.16)
- Added a print method for the output of `mahalanobis_rerun()`
  and `mahalanobis_predictors()` (0.1.4.5-0.1.4.6, 0.1.4.16, 0.1.4.18)
- Added a print method for the output of `influence_stat()`.
  (0.1.4.7)
- Provided initial support for multiple-group models
  and models with linearity constraints. (0.1.4.8 - 0.1.4.14)
- Added support for labels and `":="` to `pars_id()`. (0.1.4.13)

## Others

- Fixed the `pkgdown` site. (0.1.4.1)
- `approx_check()` will check if the model has at least one
  equality constraint. (0.1.4.2)
- Added a "Limitations" section to the vignette of the
  approximate approach. (0.1.4.2)
- Fixed the documentation of `pars_id()`. (0.1.4.12)
- Added an article for multiple-group models. (0.1.4.15)
- Restructured articles and references in the `pgkdown`
  websites. (0.1.4.15)
- In `print.lavaan_rerun()`, keep the format of lavaan
  warnings. (0.1.4.17)
- Remark that `fit_measures_change_approx()` only supports
  selected fit measures. (0.1.4.21)
- Add `cutoff_change` and `largest_change` to
  `est_change_gcd_plot()`. (0.1.4.22)
- Updated a few tests on parameters which are fixed in
  the model but their standardized versions are free.
  (0.1.4.23)
- Updated vignettes and articles with links to the
  package website. (0.1.4.24)
- Updated all vignettes for the new print methods. (0.1.4.25)

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
