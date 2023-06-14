library(testthat)
library(lavaan)
library(semfindr)

#context("Test influence_stat.R")

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
