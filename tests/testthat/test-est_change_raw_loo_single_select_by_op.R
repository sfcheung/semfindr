library(testthat)
library(lavaan)
library(semfindr)

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f3 ~  a * f1 + b * f2
ab := a * b
'

dat <- sem_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0, meanstructure = TRUE)
fit0_15 <- lavaan::sem(mod, dat0[-15, ], meanstructure = TRUE)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]

est0 <- lavaan::parameterEstimates(fit0, standardized = TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, standardized = TRUE)
est_change_rerun_test1 <- est_change_raw(rerun_out,
                                c("~"))
est_change_rerun_test2 <- est_change_raw(rerun_out,
                                c("~~"))
est_change_rerun_test3 <- est_change_raw(rerun_out,
                                c("=~"))
est_change_rerun_test4 <- est_change_raw(rerun_out,
                                c(":="))
est_change_rerun_test5 <- est_change_raw(rerun_out,
                                c(":=", "~~"))
est_change_rerun_test6 <- est_change_raw(rerun_out,
                                c("f3 ~ f2", ":=", "~~"))
est_change_rerun_test7 <- est_change_raw(rerun_out,
                                c("~1"))

est_change_rerun_std_test1 <- est_change_raw(rerun_out,
                                c("~"), standardized = TRUE)
est_change_rerun_std_test2 <- est_change_raw(rerun_out,
                                c("~~"), standardized = TRUE)
est_change_rerun_std_test3 <- est_change_raw(rerun_out,
                                c("=~"), standardized = TRUE)
est_change_rerun_std_test4 <- est_change_raw(rerun_out,
                                c(":="), standardized = TRUE)
est_change_rerun_std_test5 <- est_change_raw(rerun_out,
                                c(":=", "~~"), standardized = TRUE)
est_change_rerun_std_test6 <- est_change_raw(rerun_out,
                                c("f3 ~ f2", ":=", "~~"), standardized = TRUE)
est_change_rerun_std_test7 <- est_change_raw(rerun_out,
                                c("~1"), standardized = TRUE)

parameters_names <- paste0(est0$lhs, est0$op, est0$rhs)
parameters_names[est0$op == ":="] <- est0[est0$op == ":=", "label"]

parameters_names_paths <- parameters_names[(est0$op == "~") & !is.na(est0$z)]
parameters_names_cov <- parameters_names[(est0$op == "~~") & !is.na(est0$z)]
parameters_names_loads <- parameters_names[(est0$op == "=~") & !is.na(est0$z)]
parameters_names_def <- parameters_names[(est0$op == ":=") & !is.na(est0$z)]
parameters_names_int <- parameters_names[(est0$op == "~1") & !is.na(est0$z)]

parameters_names_paths_std <- parameters_names[est0$op == "~"]
parameters_names_cov_std <- parameters_names[est0$op == "~~"]
parameters_names_loads_std <- parameters_names[est0$op == "=~"]
parameters_names_def_std <- parameters_names[est0$op == ":="]
parameters_names_int_std <- parameters_names[est0$op == "~1"]

test_that("Parameter selected by operators", {
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test1)),
        sort(parameters_names_paths)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test2)),
        sort(parameters_names_cov)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test3)),
        sort(parameters_names_loads)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test4)),
        sort(parameters_names_def)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test5)),
        sort(c(parameters_names_def,
               parameters_names_cov))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test6)),
        sort(c(parameters_names_def,
               parameters_names_cov,
               "f3~f2"))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test7)),
        sort(parameters_names_int)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_std_test1)),
        sort(parameters_names_paths_std)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_std_test2)),
        sort(parameters_names_cov_std)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_std_test3)),
        sort(parameters_names_loads_std)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_std_test4)),
        sort(parameters_names_def_std)
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_std_test5)),
        sort(c(parameters_names_def_std,
               parameters_names_cov_std))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_std_test6)),
        sort(c(parameters_names_def_std,
               parameters_names_cov_std,
               "f3~f2"))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_std_test7)),
        sort(parameters_names_int_std)
      )
  })

# Parameters that are fixed but free in the standardized solution.

parameterEstimates(fit0, standardized = TRUE)
est_change_rerun_load_std <- est_change_raw(rerun_out,
                                c("=~"),
                                standardized = TRUE)
(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)
(est0_15$est_all_std <- est0$std.all)
(est0_15$est_std_cha <- est0_15$est_all_std - est0_15$std.all)

test_that("Check fixed parameters which is free in the standardized solution ", {
    expect_equal(ignore_attr = TRUE,
        sort(est_change_rerun_load_std[15, ]),
        sort(est0_15$est_std_cha[est0_15$op == "=~"])
      )
  })
