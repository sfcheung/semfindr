skip("WIP")
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

index_plot(fit_est_change, "gcd")
index_plot(fit_est_change_raw, "m1~iv2")
index_plot(fit_est_change_raw, "m1~iv2",
           largest_x = 5)

index_plot(inf_out, "chisq",
           x_label = "Chi-Square Influence",
           largest_x = 3,
           cutoff_x_high = .08,
           cutoff_x_low = -.25)
index_plot(inf_out, "chisq",
           x_label = "Chi-Square Influence",
           absolute = TRUE,
           largest_x = 3,
           cutoff_x_high = .08)
index_plot(inf_out, "gcd",
           x_label = "gCD")
index_plot(inf_out, "gcd",
           x_label = "gCD",
           largest_x = 3,
           cutoff_x = .05)
