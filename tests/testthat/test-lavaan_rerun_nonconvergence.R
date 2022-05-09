skip("WIP")
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
table(sapply(fit_rerun$rerun, lavInspect, what = "post.check"))

test_that("Convergence", {
    expect_equal(sum(sapply(fit_rerun$rerun, lavInspect,
                            what = "converged")),
                 19)
  })

test_that("Warnings", {
    expect_equal(sum(sapply(fit_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 2)
  })
