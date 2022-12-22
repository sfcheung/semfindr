<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/semfindr.svg)](https://github.com/sfcheung/semfindr)
[![Last Commit at Master](https://img.shields.io/github/last-commit/sfcheung/semfindr.svg)](https://github.com/sfcheung/semfindr/commits/master)
[![R-CMD-check](https://github.com/sfcheung/semfindr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/semfindr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.1.0.9012), updated on 2022-12-22, [release history](https://sfcheung.github.io/semfindr/news/index.html))

# semfindr

A find(e)r of outliers and influential cases in structural equation modeling
based mainly on the sensitivity analysis procedures presented by Pek and
MacCallum (2011).

This package supports two approaches: leave-one-out analysis and approximate case influence.

## Leave-One-Out Analysis

This approach examines the influence of each case by refitting a model with
this case removed.

Unlike other similar
packages, the workflow adopted in semfindr separates the leave-one-out
analysis (refitting a model with one case removed) from the case influence
measures.

- Users first do the leave-one-out analysis for all cases, or
cases selected based on some criteria (`vignette("selecting_cases")`), using
`lavaan_rerun()`.

- Users then compute various case influence measures
using the output of `lavaan_rerun()`.

This approaches avoids unnecessarily refitting the models for each set of
influence measures, and also allows analyzing only probable influential cases
when the model takes a long time to fit.

The functions were designed to be flexible
such that users can compute case influence measures on

- standardized parameter estimates and generalized Cook's distance for
  selected parameters;
- raw or standardized estimates;
- on any fit measures supported by `lavaan::fitMeasures()`.

This package also supports various plots to visualize
case influence, including a bubble plot similar to that by `car::influencePlot()`
All plots generated are `ggplot` plots that can be further modified by users.
More can be found in *Quick Start* (`vignette("semfindr")`).

## Approximate Case Influence

This approach computes the approximate influence of each case using *casewise*
*scores* and *casewise* *likelihood*. This method is efficient because it does
not requires refitting the model for each case. However, it can only approximate
the change. This approach can be used when the number of cases is very large
and/or the model takes a long time to fit. More can be found in the
vignette *Approximate Case Influence Using Scores and Casewise Likelihood*
(`vignette("casewise_scores")`).

# Installation

The latest version can be installed by `remotes::install_github`:

```{r}
remotes::install_github("sfcheung/semfindr")
```

You can learn more about this package at the
[Github page](https://sfcheung.github.io/semfindr/) of this
package and
Quick Start (`vignette("semfindr")`).

# Reference

Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation
models: Cases and their influence. *Multivariate Behavioral Research, 46*(2),
202-228. https://doi.org/10.1080/00273171.2011.561068
