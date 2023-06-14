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
fit0_4 <- lavaan::sem(mod, dat0[-4, ], group = "gp")
est0 <- parameterEstimates(fit0, remove.def = TRUE)
est0_4 <- parameterEstimates(fit0_4, remove.def = TRUE)
est0_4$est_all <- est0$est
est0_4$cha <- est0_4$est_all - est0_4$est
est0_4$std_cha <- est0_4$cha / est0_4$se
i_free <- which(!is.na(est0_4$z))
est0_free <- est0[i_free, ]
est0_4_free <- est0_4[i_free, ]

est_change_rerun_all <- est_change(rerun_out)
est_change_rerun_all_4 <- est_change_rerun_all[4, ]

vcov0_4 <- vcov(fit0_4)[-3, -3]
tmp <- matrix(est0_4_free$cha[-3], 13, 1)
gcd_self <- as.vector(t(tmp) %*% solve(vcov0_4) %*% tmp)

test_that("Compare standardized change for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        est0_4_free$std_cha,
        est_change_rerun_all_4[1:14]
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
