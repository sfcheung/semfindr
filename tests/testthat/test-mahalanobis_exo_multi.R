library(testthat)
library(lavaan)
library(semfindr)

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

fit0_data <- lav_data_used(fit0)

fit0_free <- lavInspect(fit0, "free")
exo_vars <- lavNames(fit0, "ov.x")
fit0_data_exo <- dat0[, exo_vars]
fit0_data_exo_g <- split(fit0_data_exo, dat0$gp)
md_predictors_check <- lapply(fit0_data_exo_g,
                          function(x) {
                              mahalanobis(x, colMeans(x), cov(x))
                            })
md_predictors_check <- unlist(md_predictors_check, use.names = FALSE)

md_predictors <- mahalanobis_predictors(fit0)

test_that("Compare Mahalanobis distances: lavaan", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors)),
        sort(md_predictors_check)
      )
  })

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

md_predictors_rerun <- mahalanobis_predictors(rerun_out)

test_that("Compare Mahalanobis distances: lavaan_rerun", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors_rerun)),
        sort(md_predictors_check)
      )
  })

# Test whether an error will a vector of NAs will be returned if
# the fit object does not have exogenous observed variables.


mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

dat <- cfa_dat

dat0 <- dat[1:100, ]
set.seed(85601)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp", group.equal = "loadings")

rerun_out <- lavaan_rerun(fit0, parallel = FALSE,
                          to_rerun = 1:5)

test_that("No exogenous observed variables", {
    expect_warning(
        md_predictors <- mahalanobis_predictors(fit0),
        "The model has no exogenous observed variables."
      )
    expect_warning(
        md_predictors_rerun <- mahalanobis_predictors(rerun_out),
        "The model has no exogenous observed variables."
      )
    expect_true(all(is.na(md_predictors)))
    expect_true(all(is.na(md_predictors_rerun)))
  })

