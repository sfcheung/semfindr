library(testthat)
library(lavaan)
library(semfindr)

#context("Test est_change_raw")

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit <- lavaan::sem(mod, dat0, group = "gp")

# From scores
fit_est_change_approx <- lavScores(fit,
                                   ignore.constraints = TRUE,
                                   remove.duplicated = FALSE) %*% vcov(fit) *
                            nobs(fit) / (nobs(fit) - 1)
# From function
fit_est_change_approx2 <- est_change_raw_approx(fit)

test_that("Checked against known results", {
    expect_equal(ignore_attr = TRUE,
        fit_est_change_approx2,
        fit_est_change_approx
      )
  })

set.seed(1314)
fit_rerun <- lavaan_rerun(fit, to_rerun = 3:5)
fit_est_change <- est_change_raw(fit_rerun)

test_that("Exact and approximate results are similar", {
    abs_diff <- abs(fit_est_change_approx2[3:5, ] -
                    fit_est_change[, 1:24])
    expect_lt(max(abs_diff), expected = .01)
  })

test1 <- est_change_raw_approx(fit, c("~"))
test2 <- est_change_raw_approx(fit, c("~~"))
test3 <- est_change_raw_approx(fit, c("m1 ~ iv1", "~~"))

test_that("est_change_raw_approx: Selected parameters", {
    expect_equal(setdiff(colnames(test1),
                  c("m1~iv1", "m1~iv2", "dv~m1",
                    "m1~iv1.g2", "m1~iv2.g2", "dv~m1.g2")),
                 character(0))
    expect_equal(setdiff(colnames(test2),
                  c("iv1~~iv2", "m1~~m1", "dv~~dv",
                    "iv1~~iv1", "iv2~~iv2",
                    "iv1~~iv2.g2", "m1~~m1.g2", "dv~~dv.g2",
                    "iv1~~iv1.g2", "iv2~~iv2.g2")),
                 character(0))
    expect_equal(setdiff(colnames(test3),
                  c("iv1~~iv2", "m1~iv1", "m1~~m1",
                    "dv~~dv", "iv1~~iv1", "iv2~~iv2",
                    "iv1~~iv2.g2", "m1~iv1.g2", "m1~~m1.g2",
                    "dv~~dv.g2", "iv1~~iv1.g2", "iv2~~iv2.g2")),
                 character(0))
  })

# With fixed parameters

mod <-
'
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit <- lavaan::sem(mod, dat0, group = "gp")
gcd_approx2 <- est_change_raw_approx(fit)

# From scores
fit_est_change_approx <- lavScores(fit,
                                   ignore.constraints = TRUE,
                                   remove.duplicated = FALSE) %*% vcov(fit) *
  nobs(fit) / (nobs(fit) - 1)
# From function
fit_est_change_approx2 <- est_change_raw_approx(fit)

test_that("Checked against known results", {
    expect_equal(ignore_attr = TRUE,
        fit_est_change_approx2,
        fit_est_change_approx
      )
  })
