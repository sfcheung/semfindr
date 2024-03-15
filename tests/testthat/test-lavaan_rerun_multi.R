library(testthat)
library(lavaan)
library(semfindr)

#context("Test lavaan_rerun")

dat <- pa_dat
dat0 <- dat[1:60, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

mod <-
'
iv1 ~~ iv2
m1 ~ c(a1, a2) * iv1 + c(NA, NA) * iv2
dv ~ c(b, b) * m1
'

fit0 <- lavaan::sem(mod, dat0, group = "gp")
parameterEstimates(fit0)
fit0_15 <- lavaan::sem(mod, dat0[-15, ], group = "gp")
parameterEstimates(fit0_15)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

rerun_15 <- rerun_out$rerun[[15]]

test_that("Compare parameter estimates of omitting an arbitrary case", {
    expect_equal(
        parameterEstimates(fit0_15),
        parameterEstimates(rerun_15),
        ignore_attr = TRUE
      )
  })

datm <- dat0[1:25, ]
datm[1, 2] <- datm[2, 3] <- datm[3, 4] <- datm[4, 1:4] <- NA
fitm <- lavaan::sem(mod, datm, group = "gp")

test_that("Works for missing data", {
    expect_no_error(rerunm_out <- lavaan_rerun(fitm))
    fitm_10 <- lavaan::sem(mod, datm[-10, ], group = "gp")
    rerunm_10 <- rerunm_out$rerun[["10"]]
    expect_equal(ignore_attr = TRUE,
        parameterEstimates(fitm_10), parameterEstimates(rerunm_10)
      )
  })
