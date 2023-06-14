skip("Equality constraints are supported in est_change()")
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
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:40, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")

rerun_out <- lavaan_rerun(fit0, parallel = FALSE,
                          to_rerun = 1:5)

test_that("Equality constraints and constrained parameters", {
  expect_message(est_change_rerun_all <- est_change(rerun_out))
  expect_true(all(is.na(est_change_rerun_all[, "gcd"])))
})
