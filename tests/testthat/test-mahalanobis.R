library(testthat)
library(lavaan)

#context("Test mahalanobis_rerun.R")

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

md_fit <- mahalanobis_rerun(fit0)


md_stats <- mahalanobis(dat0, colMeans(dat0), cov(dat0))

test_that("Compare Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_rerun),
        md_stats
      )
  })

test_that("Can accept a lavaan fit object", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_fit),
        md_stats
      )
  })
