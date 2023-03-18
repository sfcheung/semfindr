library(testthat)
library(lavaan)
library(semfindr)

# Handle a model with equality constraint

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ a2 * iv1 + a2 * iv2
dv ~ b * m1
a1b := a2 * b
'

dat <- pa_dat

dat0 <- dat
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE,
                          to_rerun = 1:5)

test_that("Equality constraints and constrained parameters", {
  expect_message(est_change_rerun_all <- est_change(rerun_out))
  expect_true(all(is.na(est_change_rerun_all[, "gcd"])))
})
