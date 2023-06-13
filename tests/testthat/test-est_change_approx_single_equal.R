skip("Equality constraints are supported in est_change_raw_approx()")
library(testthat)
library(lavaan)
library(semfindr)

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

# test_that("Error if equality constraints", {
#     expect_error(gcd_approx2 <- est_change_approx(fit0))
#   })

