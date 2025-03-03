<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/semfindr?color=blue)](https://CRAN.R-project.org/package=semfindr)
[![CRAN: Release Date](https://www.r-pkg.org/badges/last-release/semfindr?color=blue)](https://cran.r-project.org/package=semfindr)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/semfindr?color=blue)](https://r-pkg.org/pkg/semfindr)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/semfindr.svg)](https://github.com/sfcheung/semfindr)
[![Last Commit at Master](https://img.shields.io/github/last-commit/sfcheung/semfindr.svg)](https://github.com/sfcheung/semfindr/commits/master)
[![R-CMD-check](https://github.com/sfcheung/semfindr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/semfindr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.1.9, updated on 2025-03-04, [release history](https://sfcheung.github.io/semfindr/news/index.html))

# semfindr: Finding influential cases in SEM <img src="man/figures/logo.png" align="right" height="150" />

A find(e)r of influential cases in structural equation modeling
based mainly on the sensitivity analysis procedures presented by Pek and
MacCallum (2011).

This package supports two approaches: leave-one-out analysis and approximate
case influence.

## Leave-One-Out Analysis

This approach examines the influence of each case by refitting a model with
this case removed.

Unlike other similar
packages, the workflow adopted in semfindr separates the leave-one-out
analysis (refitting a model with one case removed) from the case influence
measures.

- Users first do the leave-one-out model fitting for all cases, or
cases selected based on some criteria
(`vignette("selecting_cases", package = "semfindr")`), using
`lavaan_rerun()`.

- Users then compute case influence measures
using the output of `lavaan_rerun()`.

This approaches avoids unnecessarily refitting the models for each set of
influence measures, and also allows analyzing only probable influential cases
when the model takes a long time to fit.

The functions were designed to be flexible
such that users can compute case influence measures such as

- standardized parameter estimates and generalized Cook's distance for
  selected parameters;
- changes in raw or standardized estimates of parameters;
- changes in fit measures supported by `lavaan::fitMeasures()`.

This package can also be generate plots to visualize
case influence, including a bubble plot similar to that by `car::influencePlot()`
All plots generated are `ggplot` plots that can be further modified by users.
More can be found in *Quick Start* (`vignette("semfindr", package = "semfindr")`).

## Approximate Case Influence

This approach computes the approximate influence of each case using *casewise*
*scores* and *casewise* *likelihood*. This method is efficient because it does
not requires refitting the model for each case. However, it can only approximate
the influence, unlike the leave-one-out approach, which produce exact influence.
This approach can be used when the number of cases is very large
and/or the model takes a long time to fit. Technical details can be found in the
vignette *Approximate Case Influence Using Scores and Casewise Likelihood*
(`vignette("casewise_scores", package = "semfindr")`).

# Installation

The stable version at CRAN can be installed by `install.packages()`:

```r
install.packages("semfindr")
```

The latest developmental version can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/semfindr")
```

You can learn more about this package at the
[Github page](https://sfcheung.github.io/semfindr/) of this
package and
Quick Start (`vignette("semfindr", package = "semfindr")`).

# Reference

Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation
models: Cases and their influence. *Multivariate Behavioral Research, 46*(2),
202-228. https://doi.org/10.1080/00273171.2011.561068

# Comments, Suggestions, and Bug Reports

Please post your comments, suggestions, and bug reports as issues
at [GitHub](https://github.com/sfcheung/semptools/issues), or contact
the maintainer by email. Thanks in advance for trying out `semfindr`.