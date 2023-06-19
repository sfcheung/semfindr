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

est0 <- lavaan::parameterEstimates(fit0, standardized = TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, standardized = TRUE)
est_change_rerun_all <- est_change_raw(rerun_out)
est_change_rerun_all_std <- est_change_raw(rerun_out, standardized = TRUE)
est_change_rerun_all_paths <- est_change_raw(rerun_out,
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))
est_change_rerun_all_paths_std <- est_change_raw(rerun_out,
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"),
                                standardized = TRUE)
est_change_rerun_user <- est_change_raw(rerun_out,
                                        c("a1b"))
est_change_rerun_user_std <- est_change_raw(rerun_out,
                                            c("a1b"),
                                            standardized = TRUE)

parameters_names <- gsub(" ", "", c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))

(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)
(est0_15$est_all_std <- est0$std.all)
(est0_15$est_std_cha <- est0_15$est_all_std - est0_15$std.all)

est0_15$par_names <- paste0(est0_15$lhs, est0_15$op, est0_15$rhs)

parameters_labels <- est0_15$label[est0_15$par_names %in% parameters_names]
parameters_labels <- ifelse(parameters_labels == "",
                            parameters_names,
                            parameters_labels)

est0_15_all_paths <- est0_15[est0_15$par_names %in% parameters_names, "est_cha"]
est0_15_all_paths_std <- est0_15[est0_15$par_names %in% parameters_names, "est_std_cha"]

id_free <- !is.na(est0$z) | est0$op == ":="

est0_free <- est0[id_free, ]
est0_15_free <- est0_15[id_free, ]
k <- nrow(est0_free)
k2 <- length(parameters_names)

test_that("Compare raw change in unstandardized solution for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        est0_15_free$est_cha,
        est_change_rerun_all[15, ]
      )
  })

test_that("Compare raw change in standardized solution for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        est0_15$est_std_cha,
        est_change_rerun_all_std[15, ]
      )
  })

test_that("Compare raw change in unstandardized solution for an arbitrary case, with selected parameters", {
    expect_equal(ignore_attr = TRUE,
        sort(est0_15_all_paths),
        sort(est_change_rerun_all_paths[15, ])
      )
  })

test_that("Compare raw change in standardized solution for an arbitrary case, with selected parameters", {
    expect_equal(ignore_attr = TRUE,
        sort(est0_15_all_paths_std),
        sort(est_change_rerun_all_paths_std[15, ])
      )
  })

test_that("Check user-defined parameters", {
    expect_equal(ignore_attr = TRUE,
        sort(est_change_rerun_user),
        sort(est_change_rerun_all[, "a1b"])
      )
    expect_equal(ignore_attr = TRUE,
        sort(est_change_rerun_user_std),
        sort(est_change_rerun_all_std[, "a1b"])
      )
  })

# Parameters that are fixed but free in the standardized solution.

parameterEstimates(fit0)
parameterEstimates(fit0, standardized = TRUE)
est_change_rerun_vcov_std <- est_change_raw(rerun_out,
                                c("~~"),
                                standardized = TRUE)

test_that("Check fixed parameters which is free in the standardized solution ", {
    expect_equal(ignore_attr = TRUE,
        sort(est_change_rerun_vcov_std[15, ]),
        sort(est0_15$est_std_cha[est0_15$op == "~~"])
      )
  })
