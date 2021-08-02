library(testthat)
library(lavaan)
library(semfindr)
 
# Can exclude exogenous variables that are not observed variables

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f2 ~ f1
'

dat <- cfa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

test_that("No exogenous observed variables", {
    expect_warning(
        md_exo <- mahalanobis_exo(fit0),
        "The model has no exogenous observed variables."
      )
    expect_warning(
        md_exo_rerun <- mahalanobis_exo(rerun_out),
        "The model has no exogenous observed variables."
      )
    expect_true(all(is.na(md_exo)))
    expect_true(all(is.na(md_exo_rerun)))
  })


mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5
f2 ~ f1 + x6
'

dat <- cfa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

fit0_data_exo <- dat0[, "x6", drop = FALSE]
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
