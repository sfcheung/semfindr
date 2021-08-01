library(testthat)
library(lavaan)
library(semfindr)

#context("Test fit measure changes")

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

fitm0 <- lavaan::fitMeasures(fit0)
fitm0_15 <- lavaan::fitMeasures(lavaan::sem(mod, dat0[-15, ]))
fitm_rerun <- fit_measures_change(rerun_out, fit_measures = "all")
fitm_change_rerun_15 <- fitm_rerun[15, ]
fitm_chagne_manual_15 <- as.numeric(fitm0 - fitm0_15)


test_that("Compare fit measures differences omitting an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        fitm_change_rerun_15, fitm_chagne_manual_15
      )
  })

