skip("Equality constraints are supported in est_change_raw_approx()")
library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit <- lavaan::sem(mod, dat0, group = "gp")

# test_that("Error if equality constraints", {
#     expect_error(raw_approx2 <- est_change_raw_approx(fit0))
#   })

