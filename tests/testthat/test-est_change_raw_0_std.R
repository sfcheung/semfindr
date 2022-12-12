library(testthat)
library(lavaan)
library(semfindr)

#context("Test est_change_raw")

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f2 ~ f1
'

dat <- cfa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::cfa(mod, dat0)
fit0_15 <- lavaan::cfa(mod, dat0[-15, ])

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]

est0 <- lavaan::parameterEstimates(fit0, standardized = TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, standardized = TRUE)
est_change_rerun_all_std <- est_change_raw(rerun_out, standardized = TRUE)
# est_change_rerun_all_paths_std <- est_change_raw(rerun_out, 
#                                 c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"),
#                                 standardized = TRUE)
# parameters_names <- gsub(" ", "", c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))

(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)
(est0_15$est_all_std <- est0$std.all)
(est0_15$est_std_cha <- est0_15$est_all_std - est0_15$std.all)

est0_15$par_names <- paste0(est0_15$lhs, est0_15$op, est0_15$rhs)

k <- nrow(est0)

test_that("Raw change in standardized loading in CFA", {
    expect_equal(ignore_attr = TRUE,
        est0_15$est_std_cha,  
        est_change_rerun_all_std[15, ]
      )
  })

