library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ iv1 + a2 * iv2
dv ~ b * m1
a1b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:25, ]
fit0 <- lavaan::sem(mod, dat0)
fit0_15 <- lavaan::sem(mod, dat0[-15, ])

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]

rsq <- function(object) {
    est <- lavaan::parameterEstimates(object,
                                      rsquare = TRUE,
                                      se = FALSE)
    out <- c(m1_r2 = est[(est$lhs == "m1") &
                      (est$op == "r2"), "est"],
             dv_r2 = est[(est$lhs == "dv") &
                      (est$op == "r2"), "est"])
    return(out)
  }
rsq(fit0)

chisq <- function(object) {
    lavaan::fitMeasures(object, "chisq")
  }
chisq(fit0)

est0 <- lavaan::parameterEstimates(fit0, rsquare =  TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, rsquare = TRUE)
est_change_rerun_all <- user_change_raw(rerun_out, rsq)
est_change_rerun_all_chisq <- user_change_raw(rerun_out, chisq)

(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)

(est_015_chisq_cha <- fitMeasures(fit0, "chisq") - fitMeasures(fit0_15, "chisq"))

test_that("Compare raw changes for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        est0_15[10:11, "est_cha"],
        est_change_rerun_all[15, ]
      )
    expect_equal(ignore_attr = TRUE,
        est_015_chisq_cha,
        est_change_rerun_all_chisq[15, ]
      )
  })

