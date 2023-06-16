skip("To be tested in an interactive session")
library(testthat)
library(lavaan)

#context("Test mahalanobis_rerun.R")

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

dat1 <- dat0
dat1[1, 2] <- dat1[2, 3] <- dat1[3, 4] <- dat1[4, ] <- NA
suppressWarnings(fit1 <- lavaan::sem(mod, dat1, missing = "fiml.x"))

md_rerun <- mahalanobis_rerun(rerun_out)
md_fit <- mahalanobis_rerun(fit0)
md_fit1 <- mahalanobis_rerun(fit1)
md_predictors <- mahalanobis_predictors(fit0)

md_rerun
md_fit
md_fit1
md_predictors

print(md_rerun, first = 7)
print(md_rerun, first = NULL)
print(md_fit, first = 3, sort = FALSE)
print(md_fit, first = 3, sort = TRUE, decreasing = FALSE)
print(md_fit1, first = 3, sort = TRUE, decreasing = FALSE)
print(md_predictors, first = 10)

print(md_fit1)
unclass(md_fit1)

