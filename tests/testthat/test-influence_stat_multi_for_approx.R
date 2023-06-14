library(testthat)
library(lavaan)
library(semfindr)

#context("Test influence_stat.R")

mod <-
'
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:40, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")

md_rerun   <- mahalanobis_rerun(fit0)
fm_rerun_approx   <- fit_measures_change_approx(fit0)
es_rerun_approx   <- est_change_approx(fit0)

in_rerun   <- influence_stat(fit0)

test_that("Check Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_rerun),
        in_rerun[, "md"]
      )
  })

test_that("Check changes in estimates", {
    expect_equal(ignore_attr = TRUE,
        es_rerun_approx,
        in_rerun[, colnames(es_rerun_approx)]
      )
  })

test_that("Check changes in fit measures", {
    expect_equal(ignore_attr = TRUE,
        fm_rerun_approx,
        in_rerun[, c("chisq", "cfi", "rmsea", "tli")]
      )
  })
