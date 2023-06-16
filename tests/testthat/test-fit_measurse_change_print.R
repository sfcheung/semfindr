skip("To be tested in an interactive session")
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

fitm_rerun <- fit_measures_change(rerun_out, fit_measures = "all")
fitm_approx <- fit_measures_change_approx(fit0)
fitm_rerun
fitm_approx
print(fitm_rerun, sort_by = "chisq")
print(fitm_rerun, sort_by = "chisq", first = 5)
print(fitm_rerun, sort_by = "cfi", first = 5, absolute = FALSE)

print(fitm_approx, sort_by = "chisq")
print(fitm_approx, sort_by = "chisq", first = 5)
print(fitm_approx, sort_by = "rmsea", first = 10, absolute = FALSE)
print(fitm_approx, sort_by = "rmsea", first = 10)
print(fitm_approx, sort_by = "rmsea", first = 10, increasing = TRUE)
print(fitm_approx, sort_by = "rmsea", first = 5)
print(fitm_approx, sort_by = "rmsea", first = NULL)
