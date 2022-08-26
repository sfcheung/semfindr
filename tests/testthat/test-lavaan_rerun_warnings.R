library(testthat)
library(lavaan)
library(semfindr)

#context("Test handling of reruns with warnings")

mod_p <-
'
f1 =~  .7*x1 + .6*x2 + .8*x3 + .3*x5
f2 =~  .2*x1 + .6*x4 + .8*x5 + .7*x6
f1 ~~ .2*f2
'
mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

dat <- cfa_dat[1:45, ]
fit <- lavaan::cfa(mod, dat)
#summary(fit, fit.measures = TRUE)
fit_rerun <- lavaan_rerun(fit)
fit_post_check <- sapply(fit_rerun$rerun, function(x) {
                    chk <- tryCatch(lavaan::lavTech(x, what = "post.check"),
                                    warning = function(w) w)
                    })
i_valid <- sapply(fit_post_check, isTRUE)
fit_est_change1 <- est_change(fit_rerun)
fit_est_change_raw1 <- est_change_raw(fit_rerun)
fit_est_change2 <- est_change(fit_rerun, c("f1 =~ x2", "f2 =~ x5"))
fit_est_change_raw2 <- est_change_raw(fit_rerun, c("f1 =~ x2", "f2 =~ x5"))
fit_est_change3 <- est_change(fit_rerun, c("f2 =~ x5"))
fit_est_change_raw3 <- est_change_raw(fit_rerun, c("f2 =~ x5"))
fit_fm_change1 <- fit_measures_change(fit_rerun, c("chisq", "cfi"))
fit_fm_change2 <- fit_measures_change(fit_rerun, c("tli"))

test_that("Check if lavTech post.check results are stored.", {
    expect_equal(ignore_attr = TRUE,
        fit_rerun$post_check, fit_post_check
      )
  })

test_that("Check fit_est_change1.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_est_change1), i_valid
      )
  })

test_that("Check fit_est_change_raw1.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_est_change_raw1), i_valid
      )
  })


test_that("Check fit_est_change2.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_est_change2), i_valid
      )
  })

test_that("Check fit_est_change_raw2.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_est_change_raw2), i_valid
      )
  })

test_that("Check fit_est_change3.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_est_change3), i_valid
      )
  })

test_that("Check fit_est_change_raw3.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_est_change_raw3), i_valid
      )
  })

test_that("Check fit_fm_change1.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_fm_change1), i_valid
      )
  })

test_that("Check fit_fm_change2.", {
    expect_equal(ignore_attr = TRUE,
        complete.cases(fit_fm_change2), i_valid
      )
  })
