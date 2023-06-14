library(testthat)
library(lavaan)
library(semfindr)

#context("Test influence_stat.R")

mod <-
'
m1 ~ a1 * iv1 + a2 * iv2
dv ~ b * m1
a1b := a1 * b
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE,
                          to_rerun = 1:10)

md_rerun   <- mahalanobis_rerun(fit0)
fm_rerun_approx   <- fit_measures_change_approx(fit0)
es_rerun_approx   <- est_change_approx(fit0)

in_rerun   <- influence_stat(fit0)

test_that("Check Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_rerun),
        in_rerun[, "md"]
      )
  })

test_that("Check changes in estimates", {
    expect_equal(ignore_attr = TRUE,
        es_rerun_approx,
        in_rerun[, colnames(es_rerun_approx)]
      )
  })

test_that("Check changes in fit measures", {
    expect_equal(ignore_attr = TRUE,
        fm_rerun_approx,
        in_rerun[, c("chisq", "cfi", "rmsea", "tli")]
      )
  })
