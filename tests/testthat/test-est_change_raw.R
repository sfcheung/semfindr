library(testthat)
library(lavaan)
library(semfindr)

context("Test est_change_raw")

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

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]

est0 <- lavaan::parameterEstimates(fit0, standardized = TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, standardized = TRUE)
est_change_rerun_all <- est_change_raw(rerun_out)
est_change_rerun_all_std <- est_change_raw(rerun_out, standardized = TRUE)
est_change_rerun_all_paths <- est_change_raw(rerun_out, 
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))
est_change_rerun_all_paths_std <- est_change_raw(rerun_out, 
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"),
                                standardized = TRUE)
parameters_names <- gsub(" ", "", c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))

(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)
(est0_15$est_all_std <- est0$std.all)
(est0_15$est_std_cha <- est0_15$est_all_std - est0_15$std.all)

est0_15$par_names <- paste0(est0_15$lhs, est0_15$op, est0_15$rhs)

est0_15_all_paths <- est0_15[est0_15$par_names %in% parameters_names, "est_cha"]
est0_15_all_paths_std <- est0_15[est0_15$par_names %in% parameters_names, "est_std_cha"]

k <- nrow(est0)
k2 <- length(parameters_names)

test_that("Compare raw change in unstandardized solution for an arbitrary case", {
    expect_equivalent(
        est0_15$est_cha,  
        est_change_rerun_all[15, ]
      )
  })

test_that("Compare raw change in standardized solution for an arbitrary case", {
    expect_equivalent(
        est0_15$est_std_cha,  
        est_change_rerun_all_std[15, ]
      )
  })

test_that("Compare raw change in unstandardized solution for an arbitrary case, with selected parameters", {
    expect_equivalent(
        est0_15_all_paths,
        est_change_rerun_all_paths[15, ]
      )
  })

test_that("Compare raw change in standardized solution for an arbitrary case, with selected parameters", {
    expect_equivalent(
        est0_15_all_paths_std,
        est_change_rerun_all_paths_std[15, ]
      )
  })


