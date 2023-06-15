library(testthat)
library(lavaan)
library(semfindr)

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
fit0_15 <- lavaan::sem(mod, dat0[-15, ], group = "gp")

rerun_out <- lavaan_rerun(fit0, parallel = FALSE, to_rerun = 1:15)
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
                                        "a2b")
est_change_rerun_user_std <- est_change_raw(rerun_out,
                                            "a2b",
                                            standardized = TRUE)
parameters_names <- gsub(" ", "", c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))

(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)
(est0_15$est_all_std <- est0$std.all)
(est0_15$est_std_cha <- est0_15$est_all_std - est0_15$std.all)

est0_15$par_names <- paste0(est0_15$lhs, est0_15$op, est0_15$rhs)
est0_15$coef_name <- est0_15$par_names
est0_15[est0_15$group == 2, "coef_name"] <-
  paste0(est0_15[est0_15$group == 2, "coef_name"], ".g2")
est0_15[est0_15$label == "", "label"] <-
  est0_15[est0_15$label == "", "coef_name"]

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
        sort(est_change_rerun_all[, "a2b"])
      )
    expect_equal(ignore_attr = TRUE,
        sort(est_change_rerun_user_std),
        sort(est_change_rerun_all_std[, "a2b"])
      )
  })

