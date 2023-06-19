library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ iv1 + a2 * iv2
dv ~ b * m1
a1b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
fit_est_change <- est_change(rerun_out)
fit_est_change_approx <- est_change_approx(fit)
fit_est_change_raw <- est_change_raw(rerun_out)
fit_est_change_raw_approx <- est_change_raw_approx(fit)
fit_est_change_raw_std <- est_change_raw(rerun_out,
                                         standardized = TRUE)
inf_out <- influence_stat(rerun_out)
inf_approx_out <- influence_stat(fit)

set.seed(89235)
n <- nrow(dat0)
random_case_ids <- paste0("case_", sample.int(n))
fit_est_change_approx_id <- est_change_approx(fit,
                                              case_id = random_case_ids)

tmp1 <- est_to_long(fit_est_change)
tmp2 <- est_to_long(fit_est_change_approx)
tmp3 <- est_to_long(fit_est_change_approx_id)

test_that("Test est_to_long", {
    expect_true("gcd" %in% colnames(tmp1))
    expect_true("gcd_approx" %in% colnames(tmp2))
    expect_equal(nrow(tmp1), nrow(fit_est_change) *
                            (ncol(fit_est_change) - 1))
    expect_equal(nrow(tmp2), nrow(fit_est_change_approx) *
                            (ncol(fit_est_change_approx) - 1))
  })

test_that("params_selected", {
    expect_equal(params_selected(fit_est_change, c("m1~iv1", "a2", "b")),
                 c("m1~iv1", "a2", "b"))
    expect_equal(params_selected(fit_est_change, c("a2", "m1~iv1", "b")),
                 c("m1~iv1", "a2", "b"))
    expect_equal(params_selected(fit_est_change, c("a2", "~~", "b")),
                 c("m1~~m1", "dv~~dv", "a2", "b"))
    expect_equal(params_selected(fit_est_change_approx, c("a2", "~~", "b")),
                 c("m1~~m1", "dv~~dv"))
  })

skip("To be tested in an interactive session")

params <- c("m1~iv1", "a2", "b")
est_change_gcd_plot(fit_est_change, parameters = params)
est_change_gcd_plot(fit_est_change, parameters = params, largest_gcd = 3)
est_change_gcd_plot(fit_est_change, parameters = params, cutoff_gcd = .2)
est_change_gcd_plot(fit_est_change, parameters = params, cutoff_gcd = .2,
                    cutoff_change = .05,
                    largest_change = 5)

params <- c("~")
est_change_plot(fit_est_change_raw, parameters = params)
est_change_plot(fit_est_change_raw_approx, parameters = params)
est_change_plot(fit_est_change_raw_approx, parameters = params, largest_change = 5)
est_change_plot(fit_est_change_raw_std, parameters = params, cutoff_change = .01)
est_change_plot(fit_est_change_raw_std, parameters = params, cutoff_change = .01, largest_change = 3)
est_change_plot(fit_est_change_raw)
est_change_plot(fit_est_change_raw_approx, largest_change = 3)

params <- c("m1~iv1", "~")
est_change_gcd_plot(fit_est_change_approx, parameters = params)
est_change_gcd_plot(fit_est_change_approx, parameters = params, cutoff_gcd = .1)
est_change_gcd_plot(fit_est_change_approx, parameters = params, cutoff_gcd = .2)
est_change_gcd_plot(fit_est_change_approx, parameters = params, largest_gcd = 5)
est_change_gcd_plot(fit_est_change_approx, parameters = params, largest_gcd = 5, cutoff_gcd = .3)

# Test the functions on the output of influence_stat()

params <- c("m1~iv1", "a2", "b")
est_change_gcd_plot(inf_out, parameters = params)
est_change_gcd_plot(fit_est_change, parameters = params)
est_change_plot(inf_out, parameters = params)
est_change_plot(fit_est, parameters = params)
params <- c("~")
est_change_gcd_plot(inf_approx_out, parameters = params)
est_change_gcd_plot(fit_est_change_approx, parameters = params)
est_change_plot(inf_approx_out, parameters = params)
est_change_plot(fit_est_change_approx, parameters = params)

# CFA model with selected loadings

mod <-
'
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f1 ~~ f2
'

dat <- cfa_dat

dat0 <- dat[1:50, ]
fit <- lavaan::cfa(mod, dat0)

fit_est_change_approx <- est_change_approx(fit, parameters = "=~")

params <- c("f1=~x2", "f2=~x5")
est_change_gcd_plot(fit_est_change_approx, parameters = params)
est_change_gcd_plot(fit_est_change_approx, parameters = "=~", cutoff_gcd = 1)
est_change_gcd_plot(fit_est_change_approx, parameters = params, cutoff_gcd = .2)
est_change_gcd_plot(fit_est_change_approx, parameters = params, largest_gcd = 5)
est_change_gcd_plot(fit_est_change_approx, parameters = params, largest_gcd = 5, cutoff_gcd = .3)

