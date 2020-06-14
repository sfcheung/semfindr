library(testthat)
library(lavaan)
library(semfindr)

context("Test est_change")

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

est0 <- lavaan::parameterEstimates(fit0)
est0_15 <- lavaan::parameterEstimates(fit0_15)
est_change_rerun_all <- est_change(rerun_out)
est_change_rerun_all_paths <- est_change(rerun_out, 
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))
parameters_names <- gsub(" ", "", c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))

est0_15$est_all <- est0$est
est0_15$std_cha <- (est0_15$est_all - est0_15$est)/est0_15$se

est0_15$par_names <- paste0(est0_15$lhs, est0_15$op, est0_15$rhs)

est0_15_all_paths <- est0_15[est0_15$par_names %in% parameters_names, "std_cha"]

k <- nrow(est0)
k2 <- length(parameters_names)

est0_15_v <- matrix(est0$est - est0_15$est, k, 1)
est0_15_vcov <- vcov(fit0_15)
class(est0_15_vcov) <- "matrix"
est0_15_gcd <- t(est0_15_v) %*% solve(est0_15_vcov) %*% est0_15_v
est0_15_gcd
est_change_rerun_all[15, "gcd"]

test_that("Compare standardized change for an arbitrary case", {
    expect_equivalent(
        (est0$est - est0_15$est)/est0_15$se,  
        est_change_rerun_all[15, seq_len(k)]
      )
  })

test_that("Compare standardized change for an arbitrary case, with selected parameters", {
    expect_equivalent(
        est0_15_all_paths,
        est_change_rerun_all_paths[15, seq_len(k2)]
      )
  })

test_that("Compare generalized Cook's distance for for an arbitrary case", {
    expect_equivalent(
        as.vector(est0_15_gcd),
        est_change_rerun_all[15, "gcd"]
      )
  })

