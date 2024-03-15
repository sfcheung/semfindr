library(testthat)
library(lavaan)
library(semfindr)

#context("Test lavaan_rerun")

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

test_that("Compare parameter estimates of omitting an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        parameterEstimates(fit0_15), parameterEstimates(rerun_15)
      )
  })

datm <- dat[1:20, ]
datm[1, 2] <- datm[2, 3] <- datm[3, 4] <- datm[4, ] <- NA
fitm <- lavaan::sem(mod, datm)

test_that("Works for missing data", {
    expect_no_error(rerunm_out <- lavaan_rerun(fitm))
    fitm_10 <- lavaan::sem(mod, datm[-10, ])
    rerunm_10 <- rerunm_out$rerun[["10"]]
    expect_equal(ignore_attr = TRUE,
        parameterEstimates(fitm_10), parameterEstimates(rerunm_10)
      )
  })

