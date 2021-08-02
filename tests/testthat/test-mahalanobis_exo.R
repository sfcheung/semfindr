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

# Test whether an error will a vector of zero length will be returned if
# the fit object does not have exogenous observed variables.


mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
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
