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

dat0 <- dat[1:20, ]
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

parameters_labels <- est0_15$label[est0_15$par_names %in% parameters_names]
parameters_labels <- ifelse(parameters_labels == "",
                            parameters_names,
                            parameters_labels)

est0_15_all_paths <- est0_15[est0_15$par_names %in% parameters_names, "std_cha"]

id_free <- !is.na(est0$z) & est0$op != ":="

est0_free <- est0[id_free, ]
est0_15_free <- est0_15[id_free, ]
k <- nrow(est0_free)
k2 <- length(parameters_names)

est0_15_v <- matrix(est0_free$est - est0_15_free$est, k, 1)
est0_15_v <- est0_15_v[!is.na(est0_free$z)]
est0_15_vcov <- vcov(fit0_15)
class(est0_15_vcov) <- "matrix"
est0_15_gcd <- t(est0_15_v) %*% solve(est0_15_vcov) %*% est0_15_v
est0_15_gcd
est_change_rerun_all[15, "gcd"]

test_that("Compare standardized change for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        (est0_free$est - est0_15_free$est)/est0_15_free$se,
        est_change_rerun_all[15, seq_len(k)]
      )
  })

test_that("Compare standardized change for an arbitrary case, with selected parameters", {
    expect_equal(ignore_attr = TRUE,
        est0_15_all_paths,
        est_change_rerun_all_paths[15, parameters_labels]
      )
  })

test_that("Compare generalized Cook's distance for for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        as.vector(est0_15_gcd),
        est_change_rerun_all[15, "gcd"]
      )
  })

test_that("User parameters should return error or excluded", {
    expect_error(est_change(rerun_out, "a1b"))
    expect_equal(intersect(colnames(est_change(rerun_out, c("m1 ~ iv1", "a1b"))), "a1b"),
                 character(0))
  })
