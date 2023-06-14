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

dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")

rerun_out <- lavaan_rerun(fit0, to_rerun = c(1, 3, 9, 15, 50), parallel = FALSE)
rerun_15 <- rerun_out$rerun[[4]]

md_rerun   <- mahalanobis_rerun(rerun_out)
fm_rerun   <- fit_measures_change(rerun_out)
es_rerun   <- est_change(rerun_out)

in_rerun   <- influence_stat(rerun_out)

test_that("Check Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_rerun),
        in_rerun[, "md"]
      )
  })

test_that("Check changes in estimates", {
    expect_equal(ignore_attr = TRUE,
        es_rerun,
        in_rerun[, colnames(es_rerun)]
      )
  })

test_that("Check changes in fit measures", {
    expect_equal(ignore_attr = TRUE,
        fm_rerun,
        in_rerun[, c("chisq", "cfi", "rmsea", "tli")]
      )
  })
