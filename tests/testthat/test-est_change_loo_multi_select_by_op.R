library(testthat)
library(lavaan)
library(semfindr)

# An SEM model with latent factors
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f3 ~  a * f1 + b * f2
ab := a * b
'

dat <- sem_dat

dat0 <- dat[1:200, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, meanstructure = TRUE,
                    group = "gp", group.equal = "loadings")
fit0_15 <- lavaan::sem(mod, dat0[-15, ], meanstructure = TRUE)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE,
                          to_rerun = c(1:15))
rerun_15 <- rerun_out$rerun[[15]]

est0 <- lavaan::parameterEstimates(fit0, standardized = TRUE)

est_change_rerun_test1 <- est_change(rerun_out,
                                c("~"))
est_change_rerun_test2 <- est_change(rerun_out,
                                c("~~"))
est_change_rerun_test3 <- est_change(rerun_out,
                                c("=~"))
# est_change_rerun_test4 <- est_change(rerun_out,
#                                 c(":="))
est_change_rerun_test5 <- est_change(rerun_out,
                                c("=~", "~~"))
est_change_rerun_test6 <- est_change(rerun_out,
                                c("f3 ~ f2", "=~", "~~"))
est_change_rerun_test7 <- est_change(rerun_out,
                                c("~1"))
# Address issue 87
est_change_rerun_test_87_1 <- est_change(rerun_out,
                                c("=~.g2", "~~"))
est_change_rerun_test_87_2 <- est_change(rerun_out,
                                c("~~.g2"))
est_change_rerun_test_87_3 <- est_change(rerun_out,
                                c("=~.g1", "~1", "f3 ~ f2.g2"))


parameters_names <- paste0(est0$lhs, est0$op, est0$rhs)
parameters_names[est0$group == 2] <- paste0(parameters_names[est0$group == 2], ".g2")
parameters_names_no_user_labels <- parameters_names
# Use label if available
tmp <- (est0$label != "") & !grepl(".p", est0$label)
parameters_names[tmp] <- est0$label[tmp]

parameters_names_paths <- parameters_names[(est0$op == "~") & !is.na(est0$z)]
parameters_names_cov <- parameters_names[(est0$op == "~~") & !is.na(est0$z)]
parameters_names_loads <- parameters_names[(est0$op == "=~") & !is.na(est0$z)]
parameters_names_def <- parameters_names[(est0$op == ":=") & !is.na(est0$z)]
parameters_names_int <- parameters_names[(est0$op == "~1") & !is.na(est0$z)]

parameters_names_87_1 <- c(parameters_names[(est0$op == "=~") &
                                            !is.na(est0$z) &
                                            est0$group == 2],
                           parameters_names[(est0$op == "~~") &
                                             !is.na(est0$z)])
parameters_names_87_2 <- c(parameters_names[(est0$op == "~~") &
                                            !is.na(est0$z) &
                                            est0$group == 2])
parameters_names_87_3 <- c(parameters_names[(est0$op == "=~") &
                                            !is.na(est0$z) &
                                            est0$group == 1],
                           parameters_names[(est0$op == "~1") &
                                            !is.na(est0$z)],
                           parameters_names[(est0$op == "~") &
                                            (est0$lhs == "f3") &
                                            (est0$rhs == "f2") &
                                            est0$group == 2])

test_that("Parameter selected by operators", {
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test1)),
        sort(c("gcd", parameters_names_paths))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test2)),
        sort(c("gcd", parameters_names_cov))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test3)),
        sort(c("gcd", parameters_names_loads))
      )
    # expect_equal(ignore_attr = TRUE,
    #     sort(colnames(est_change_rerun_test4)),
    #     sort(parameters_names_def)
    #   )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test5)),
        sort(c(c("gcd",
               parameters_names_loads,
               parameters_names_cov)))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test6)),
        sort(c("gcd",
               parameters_names_loads,
               parameters_names_cov,
               "b", "b"))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test7)),
        sort(c("gcd", parameters_names_int))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test_87_1)),
        sort(c("gcd", parameters_names_87_1))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test_87_2)),
        sort(c("gcd", parameters_names_87_2))
      )
    expect_equal(ignore_attr = TRUE,
        sort(colnames(est_change_rerun_test_87_3)),
        sort(c("gcd", parameters_names_87_3))
      )
  })
