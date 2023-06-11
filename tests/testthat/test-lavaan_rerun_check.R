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
(out_01 <- lavaan_rerun_check(fit01))

suppressWarnings(fit05 <- lavaan::sem(mod, dat, group = "gp"))
(out_05 <- lavaan_rerun_check(fit05))

fit01_cov <- lavaan::sem(mod, sample.cov = cov(dat[, -7]), sample.nobs = nrow(dat))
(out_01_cov <- lavaan_rerun_check(fit01_cov))

test_that("Check against the flags", {
    expect_true(
        out_01 == 0
      )
  })

# Multiple group models are now supported
# test_that("Check against the flags", {
#     expect_true(
#         out_05 == -2
#       )
#   })

test_that("Check against the flags", {
    expect_true(
        out_01_cov == -1
      )
  })

# Multiple group models are now supported
# test_that("Check against the flags", {
#     expect_error(
#         lavaan_rerun(fit05)
#       )
#   })

test_that("Check against the flags", {
    expect_error(
        lavaan_rerun(fit01_cov)
      )
  })

