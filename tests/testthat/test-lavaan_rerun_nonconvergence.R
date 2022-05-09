library(testthat)
library(lavaan)
library(semfindr)

mod <- 
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'
dat <- cfa_dat[1:50, ]

fit <- lavaan::cfa(mod, dat, control = list(iter.max = 39))
lavInspect(fit, "iterations")

# This rerun has both runs that failed to converge and runs that failed post.check.
fit_rerun <- lavaan_rerun(fit, to_rerun = 1:20)
table(sapply(fit_rerun$rerun, lavInspect, what = "converged"))
suppressWarnings(table(sapply(fit_rerun$rerun, lavInspect, what = "post.check")))

test_that("Convergence", {
    expect_equal(sum(sapply(fit_rerun$rerun, lavInspect,
                            what = "converged")),
                 19)
    expect_true(all(fit_rerun$converged[-3]))
  })

test_that("Warnings", {
    expect_equal(sum(sapply(fit_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 2)
    expect_true(all(sapply(fit_rerun$post_check, inherits,
                            what = "simpleWarning")[c(1, 20)]))
  })

test_that("est_change_raw", {
    tmp <- est_change_raw(fit_rerun)
    expect_true(all(!complete.cases(tmp)[c(1, 3, 20)]))
  })

test_that("est_change", {
    tmp <- est_change(fit_rerun)
    expect_true(all(!complete.cases(tmp)[c(1, 3, 20)]))
  })

test_that("fit_measures_change", {
    tmp <- fit_measures_change(fit_rerun, fit_measures = "chisq")
    expect_equal(which(is.na(tmp)),
                 c(1, 3, 20))
  })

