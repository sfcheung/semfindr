library(testthat)
library(lavaan)
library(semfindr)

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
dat0 <- dat[1:60, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")
fit0_15 <- lavaan::sem(mod, dat0[-15, ], group = "gp")

rerun_out <- lavaan_rerun(fit0, to_rerun = c(1, 3, 9, 15, 50), parallel = FALSE)
rerun_15 <- rerun_out$rerun[["15"]]

test_that("Compare parameter estimates of omitting an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        parameterEstimates(fit0_15), parameterEstimates(rerun_15)
      )
  })

test_that("Check the number of reruns", {
    expect_equal(length(rerun_out$rerun), 5)
  })

test_that("Check the names of reruns", {
    expect_equal(names(rerun_out$rerun), as.character(c(1, 3, 9, 15, 50)))
  })

test_that("Check selected", {
    expect_equal(rerun_out$selected, c(1, 3, 9, 15, 50))
  })

# With case_id

dat <- pa_dat

dat0 <- dat[1:60, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")

set.seed(45345)
case_id_test <- paste0(sample(letters, nrow(dat0), replace = TRUE),
                       sample(letters, nrow(dat0), replace = TRUE))
case_id_to_rerun <- case_id_test[c(2, 5, 50, 14)]
rerun_out <- lavaan_rerun(fit0, case_id = case_id_test,
                                to_rerun = case_id_to_rerun, parallel = FALSE)

id_test <- which(case_id_test %in% case_id_to_rerun)[3]
fit0_test <- lavaan::sem(mod, dat0[-id_test, ], group = "gp")

rerun_test <- rerun_out$rerun[[case_id_test[14]]]

test_that("Compare parameter estimates of omitting an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        parameterEstimates(fit0_test), parameterEstimates(rerun_test)
      )
  })

test_that("Check the number of reruns", {
    expect_equal(length(rerun_out$rerun), 4)
  })

test_that("Check the names of reruns", {
    expect_equal(names(rerun_out$rerun), case_id_to_rerun)
  })

test_that("Check selected", {
    expect_equal(case_id_test[rerun_out$selected], case_id_to_rerun)
  })

# Listwise

dat0 <- dat[1:60, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[5, 1:4] <- NA
fit0 <- lavaan::sem(mod, dat0, group = "gp")

set.seed(4345)
case_id_test <- paste0(sample(letters, nrow(dat0), replace = TRUE),
                       sample(letters, nrow(dat0), replace = TRUE))
case_id_to_rerun <- case_id_test[c(6, 4, 7)]
rerun_out <- lavaan_rerun(fit0, case_id = case_id_test,
                                to_rerun = case_id_to_rerun, parallel = FALSE)
id_test <- which(case_id_test %in% case_id_to_rerun)[3]
fit0_test <- lavaan::sem(mod, dat0[-id_test, ], group = "gp")

rerun_test <- rerun_out$rerun[[case_id_test[id_test]]]

test_that("Compare parameter estimates of omitting an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        parameterEstimates(fit0_test), parameterEstimates(rerun_test)
      )
  })