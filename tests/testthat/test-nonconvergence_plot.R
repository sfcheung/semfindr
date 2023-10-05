skip_on_cran()
# Slow tests

library(testthat)
library(lavaan)

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'
set.seed(154151)
dat <- HolzingerSwineford1939[sample.int(301, 60), ]

fit <- suppressWarnings(lavaan::cfa(mod, dat))

fit_rerun <- lavaan_rerun(fit, to_rerun = 1:20, allow_inadmissible = TRUE, parallel = FALSE)

fit_rerun
out <- influence_stat(fit_rerun)

p <- gcd_plot(out)
test_that("NA in rerun", {
    expect_equal(nrow(p$data), 19)
    # expect_no_warning(print(p))
  })

p <- md_plot(out)
test_that("NA in rerun", {
    expect_equal(nrow(p$data), 20)
    # expect_no_warning(print(p))
  })

p <- gcd_gof_plot(out, "chisq")
test_that("NA in rerun", {
    expect_equal(nrow(p$data), 19)
    # expect_no_warning(print(p))
  })

p <- gcd_gof_md_plot(out, "chisq", circle_size = 35)
test_that("NA in rerun", {
    expect_equal(nrow(p$data), 19)
    # expect_no_warning(print(p))
  })

out <- est_change(fit_rerun)

p <- est_change_plot(out)
test_that("NA in rerun", {
    expect_equal(nrow(p$data), 247)
    # expect_no_warning(print(p))
  })

p <- est_change_gcd_plot(out)
test_that("NA in rerun", {
    expect_equal(nrow(p$data), 247)
    # expect_no_warning(print(p))
  })

p <- index_plot(out, "f1=~x3")
test_that("NA in rerun", {
    expect_equal(nrow(p$data), 19)
    # expect_no_warning(print(p))
  })
