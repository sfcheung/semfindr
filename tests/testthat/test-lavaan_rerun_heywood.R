library(testthat)
library(lavaan)
library(semfindr)

mod <- 
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

suppressWarnings(fit <- lavaan::cfa(mod, cfa_dat_heywood))

attr(lavaan_rerun_check(fit), "info")

test_that("Reject inadmissible solution", {
    expect_error(lavaan_rerun(fit))
  })

fit_rerun <- lavaan_rerun(fit, allow_inadmissible = TRUE)

test_that("Warnings", {
    expect_equal(sum(sapply(fit_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 26)
  })

dat2 <- cfa_dat_heywood[-1, ]
fit2 <- lavaan::cfa(mod, dat2)
fit2_rerun <- lavaan_rerun(fit2, allow_inadmissible = TRUE)

test_that("Warnings", {
    expect_equal(sum(sapply(fit2_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 0)
  })
