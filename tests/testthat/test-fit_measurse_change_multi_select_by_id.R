library(testthat)
library(lavaan)
library(semfindr)

#context("Test fit measure changes")

mod <-
'
iv1 ~~ iv2
m1 ~ c(a1, a2) * iv1 + c(NA, NA) * iv2
dv ~ c(b, b) * m1
'

dat <- pa_dat
dat0 <- dat[1:60, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")
fit0_15 <- lavaan::sem(mod, dat0[-15, ], group = "gp")

rerun_out <- lavaan_rerun(fit0, to_rerun = c(1, 3, 9, 15, 50), parallel = FALSE)
rerun_15 <- rerun_out$rerun[[4]]

fitm0 <- lavaan::fitMeasures(fit0)
fitm0_15 <- lavaan::fitMeasures(lavaan::sem(mod, dat0[-15, ], group = "gp"))
fitm_rerun <- fit_measures_change(rerun_out, fit_measures = "all")
fitm_change_rerun_15 <- fitm_rerun[4, ]
fitm_chagne_manual_15 <- as.numeric(fitm0 - fitm0_15)


test_that("Compare fit measures differences omitting an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        fitm_change_rerun_15, fitm_chagne_manual_15
      )
  })

