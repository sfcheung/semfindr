library(testthat)
library(lavaan)

#context("Test mahalanobis_rerun.R")

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

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

md_rerun <- mahalanobis_rerun(rerun_out)

md_fit <- mahalanobis_rerun(fit0)

dat0_x_g1 <- dat0[dat0$gp == "gp2", 1:4]
md_stats1 <- mahalanobis(dat0_x_g1, colMeans(dat0_x_g1), cov(dat0_x_g1))
dat0_x_g2 <- dat0[dat0$gp == "gp1", 1:4]
md_stats2 <- mahalanobis(dat0_x_g2, colMeans(dat0_x_g2), cov(dat0_x_g2))
md_stats <- c(md_stats1, md_stats2)

test_that("Compare Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_rerun)),
        sort(md_stats)
      )
  })

test_that("Can accept a lavaan fit object", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_fit)),
        sort(md_stats)
      )
  })
