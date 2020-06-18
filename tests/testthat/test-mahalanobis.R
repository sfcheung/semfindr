library(testthat)
library(lavaan)
library(semfindr)

context("Test mahalanobis_rerun.R")

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

md_rerun <- mahalanobis_rerun(rerun_out)

md_stats <- mahalanobis(dat0, colMeans(dat0), cov(dat0))

test_that("Compare Mahalanobis distances", {
    expect_equivalent(
        as.vector(md_rerun),  
        md_stats
      )
  })
