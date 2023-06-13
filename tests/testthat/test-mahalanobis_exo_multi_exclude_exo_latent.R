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

dat0 <- dat[1:100, ]
set.seed(8560)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp", group.equal = "loadings")

rerun_out <- lavaan_rerun(fit0, parallel = FALSE, to_rerun = 1:5)

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


mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5
f2 ~ f1 + x6
'

dat <- cfa_dat

dat0 <- dat[1:60, ]
set.seed(8560)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp", group.equal = "loadings")

rerun_out <- lavaan_rerun(fit0, parallel = FALSE, to_rerun = 1:5)

fit0_data_exo <- dat0[, c("x6"), drop = FALSE]
fit0_data_exo_g <- split(fit0_data_exo, dat0$gp)
md_predictors_check <- lapply(fit0_data_exo_g, function(x) {
                                  mahalanobis(x, colMeans(x), cov(x))
                                })
j <- order(as.numeric(unlist(sapply(fit0_data_exo_g, rownames))))
md_predictors_check <- unlist(md_predictors_check)[j]

md_predictors <- mahalanobis_predictors(fit0)

md_predictors_rerun <- mahalanobis_predictors(rerun_out)

test_that("Compare Mahalanobis distances: lavaan_rerun", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors)),
        sort(md_predictors_check)
      )
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors_rerun)),
        sort(md_predictors_check[1:5])
      )
  })
