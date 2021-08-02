library(testthat)
library(lavaan)
library(semfindr)

# Can identify observed variables that affect latent variables

mod <-
'
f1 =~ x1 + x2 + x3
f1 ~ x4 + x5
'

dat <- cfa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

fit0_free <- lavInspect(fit0, "free")
i <- apply(fit0_free$beta, 1, function(x) all(x == 0))
exo_vars <- names(i)[i]
fit0_data_exo <- dat0[, exo_vars]
md_exo_check <- mahalanobis(fit0_data_exo,
                      colMeans(fit0_data_exo),
                      cov(fit0_data_exo))

md_exo <- mahalanobis_exo(fit0)

md_exo_rerun <- mahalanobis_exo(rerun_out)

test_that("Compare Mahalanobis distances: lavaan_rerun", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_exo_rerun),
        md_exo_check
      )
    expect_equal(ignore_attr = TRUE,
        as.vector(md_exo_rerun),
        md_exo_check
      )
  })

