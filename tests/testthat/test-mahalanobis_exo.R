library(testthat)
library(lavaan)
library(semfindr)

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)

fit0_data <- lavInspect(fit0, "data")
colnames(fit0_data) <- lavNames(fit0)
head(fit0_data)

fit0_free <- lavInspect(fit0, "free")
i <- apply(fit0_free$beta, 1, function(x) all(x == 0))
exo_vars <- names(i)[i]
fit0_data_exo <- dat0[, exo_vars]
md_exo_check <- mahalanobis(fit0_data_exo,
                      colMeans(fit0_data_exo),
                      cov(fit0_data_exo))

md_exo <- mahalanobis_exo(fit0)

test_that("Compare Mahalanobis distances: lavaan", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_exo),
        md_exo_check
      )
  })

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

md_exo_rerun <- mahalanobis_exo(rerun_out)

test_that("Compare Mahalanobis distances: lavaan_rerun", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_exo_rerun),
        md_exo_check
      )
  })
