library(testthat)
library(lavaan)
library(semfindr)

# This test is not thorough. Just a draft.

dat <- cfa_dat

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~~ f2
"

dat_gp <- dat
dat$gp <- rep(c("gp1", "gp2"), length.out = nrow(dat_gp))

fit01 <- lavaan::sem(mod, dat)
(out_01 <- approx_check(fit01))

suppressWarnings(fit05 <- lavaan::sem(mod, dat, group = "gp"))
(out_05 <- approx_check(fit05))

fit01_cov <- lavaan::sem(mod, sample.cov = cov(dat[, -7]), sample.nobs = nrow(dat))
(out_01_cov <- approx_check(fit01_cov))

fit01_mlr <- lavaan::sem(mod, dat, estimator = "mlr")
(out_01_mlr <- approx_check(fit01_mlr))

test_that("Check against the flags", {
    expect_true(
        out_01 == 0
      )
  })

test_that("Check against the flags", {
    expect_true(
        out_05 == -1
      )
  })

test_that("Check against the flags", {
    expect_true(
        out_01_cov == -1
      )
  })

test_that("Check against the flags", {
    expect_true(
        out_01_mlr == -1
      )
  })

test_that("Check against the flags", {
    expect_error(
        est_change_approx(fit05)
      )
    expect_error(
        est_change_raw_approx(fit05)
      )
    expect_error(
        fit_measures_change_approx(fit05)
      )
  })

test_that("Check against the flags", {
    expect_error(
        est_change_approx(fit01_cov)
      )
    expect_error(
        est_change_raw_approx(fit01_cov)
      )
    expect_error(
        fit_measures_change_approx(fit01_cov)
      )
  })

test_that("Check against the flags", {
    expect_error(
        est_change_approx(fit01_mlr)
      )
    expect_error(
        est_change_raw_approx(fit01_mlr)
      )
    expect_error(
        fit_measures_change_approx(fit01_mlr)
      )
  })
