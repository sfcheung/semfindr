library(testthat)
library(lavaan)

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
fit_est_change_raw <- est_change_raw(rerun_out)
inf_out <- influence_stat(rerun_out)

p <- index_plot(fit_est_change, "gcd")
test_that("index_plot", {
    expect_equal(p$data$x,
                 fit_est_change[, "gcd"],
                 ignore_attr = TRUE)
    expect_equal(p$layers[[4]]$data[1, "row_id"], 9)
  })

p <- index_plot(fit_est_change_raw, "m1~iv2")
test_that("index_plot", {
    expect_equal(p$data$x,
                 fit_est_change_raw[, "m1~iv2"],
                 ignore_attr = TRUE)
    expect_equal(p$layers[[4]]$data[1, "row_id"], 8)
  })

p <- index_plot(fit_est_change_raw, "m1~iv2",
                largest_x = 5)
test_that("index_plot", {
    expect_equal(p$data$x,
                 fit_est_change_raw[, "m1~iv2"],
                 ignore_attr = TRUE)
    expect_equal(nrow(p$layers[[4]]$data), 5)
    expect_equal(p$labels$y, "Statistic")
  })

p <- index_plot(inf_out, "chisq",
                x_label = "Chi-Square Influence",
                largest_x = 3,
                cutoff_x_high = .08,
                cutoff_x_low = -.25)
test_that("index_plot", {
    expect_equal(p$data$x,
                 inf_out[, "chisq"],
                 ignore_attr = TRUE)
    expect_equal(p$layers[[4]]$data[1, 1], -.25)
    expect_equal(p$layers[[5]]$data[1, 1], .08)
    expect_equal(nrow(p$layers[[6]]$data), 6)
    expect_equal(p$labels$y, "Chi-Square Influence")
  })

p <- index_plot(inf_out, "chisq",
                absolute = TRUE,
                largest_x = 3,
                cutoff_x_high = .08)
test_that("index_plot", {
    expect_equal(p$data$x,
                 abs(inf_out[, "chisq"]),
                 ignore_attr = TRUE)
    expect_equal(p$layers[[4]]$data[1, 1], .08)
    expect_equal(nrow(p$layers[[5]]$data), 6)
    expect_equal(p$labels$y, "Absolute(Statistic)")
  })
