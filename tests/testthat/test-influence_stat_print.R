skip("To be tested in an interactive session")
library(testthat)
library(lavaan)
library(semfindr)

# NOTE
# Do this after the print methods for
# other functions it calls are ready.

#context("Test influence_stat.R")

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

in_rerun   <- influence_stat(rerun_out)
print(in_rerun, first = 10)
print(in_rerun, first = 10, what = "parameters")
print(in_rerun, first = 10, what = "fit_measures")
print(in_rerun, first = 10, what = "mahalanobis")
print(in_rerun, first = 10, what = c("parameters", "mahalanobis"))
print(in_rerun, first = 10, what = c("parameters", "fit_measures"), sort_fit_measures_by = "chisq", sort_parameters_by = "est")
