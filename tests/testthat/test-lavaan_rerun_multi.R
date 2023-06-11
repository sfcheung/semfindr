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

