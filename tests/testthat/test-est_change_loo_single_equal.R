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
m1 ~ a1 * iv1 + a2 * iv2
dv ~ a2 * m1
a1a2 := a1 * a2
'

dat <- pa_dat

dat0 <- dat
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE,
                          to_rerun = 1:5)
fit0_4 <- lavaan::sem(mod, dat0[-4, ])
est0 <- parameterEstimates(fit0)
est0_4 <- parameterEstimates(fit0_4)
est0_4$est_all <- est0$est
est0_4$cha <- est0_4$est_all - est0_4$est
est0_4$std_cha <- est0_4$cha / est0_4$se
est0_free <- est0[1:5, ]
est0_4_free <- est0_4[1:5, ]

est_change_rerun_all <- est_change(rerun_out)
est_change_rerun_all_4 <- est_change_rerun_all[4, ]

vcov0_4 <- vcov(fit0_4)[-2, -2]
tmp <- matrix(est0_4_free$cha[-2], 4, 1)
gcd_self <- as.vector(t(tmp) %*% solve(vcov0_4) %*% tmp)

test_that("Compare standardized change for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        est0_4_free$std_cha,
        est_change_rerun_all_4[1:5]
      )
  })

test_that("Compare generalized Cook's distance for for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        gcd_self,
        as.vector(est_change_rerun_all_4["gcd"])
      )
  })

# test_that("Equality constraints and constrained parameters", {
#   expect_message(est_change_rerun_all <- est_change(rerun_out))
#   expect_true(all(is.na(est_change_rerun_all[, "gcd"])))
# })
