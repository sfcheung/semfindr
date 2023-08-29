skip_if_not_installed("modi")
library(testthat)
library(lavaan)
library(semfindr)

# Can handle missing data

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[4, ] <- NA
head(dat0)
suppressWarnings(fit0 <- lavaan::sem(mod, dat0, missing = "fiml.x"))

fit0_data <- lavInspect(fit0, "data")
colnames(fit0_data) <- lavNames(fit0)
head(fit0_data)

fit0_free <- lavInspect(fit0, "free")
i <- apply(fit0_free$beta, 1, function(x) all(x == 0))
exo_vars <- names(i)[i]
fit0_data_exo <- dat0[, exo_vars]
suppressWarnings(em_out_fit <- lavaan::lavCor(fit0_data_exo,
                                              missing = "fiml",
                                              output = "fit"))
em_out <- list(param = list())
tmp <- lavaan::lavInspect(em_out_fit, "implied")
em_out$param$beta <- tmp$mean
em_out$param$sigma <- tmp$cov

md_predictors_check <- modi::MDmiss(fit0_data_exo,
                      em_out$param$beta,
                      em_out$param$sigma)

md_predictors <- mahalanobis_predictors(fit0)

test_that("Compare Mahalanobis distances: lavaan", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_predictors),
        md_predictors_check
      )
  })

suppressWarnings(rerun_out <- lavaan_rerun(fit0, parallel = FALSE))

md_predictors_rerun <- mahalanobis_predictors(rerun_out)

test_that("Compare Mahalanobis distances: lavaan_rerun", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_predictors_rerun),
        md_predictors_check
      )
  })
