library(testthat)
library(lavaan)
library(semfindr)

#context("Test est_change_raw")


mod <- 
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)
fit0_15 <- lavaan::sem(mod, dat0[-15, ])

rerun_out <- lavaan_rerun(fit0, to_rerun = c(1, 3, 9, 15, 50), parallel = FALSE)
rerun_15 <- rerun_out$rerun[[4]]

est0 <- lavaan::parameterEstimates(fit0, standardized = TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, standardized = TRUE)
est_change_rerun_all <- est_change_raw(rerun_out)

(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)

est0_15$par_names <- paste0(est0_15$lhs, est0_15$op, est0_15$rhs)

k <- nrow(est0)

test_that("Compare raw change in unstandardized solution for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        est0_15$est_cha,  
        est_change_rerun_all[4, ]
      )
  })

