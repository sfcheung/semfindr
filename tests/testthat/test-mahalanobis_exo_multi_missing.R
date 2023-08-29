skip_if_not_installed("modi")
library(testthat)
library(lavaan)
library(semfindr)

# Can handle missing data

mod <-
'
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:40, ]
dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[4, 1:4] <- NA
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)
head(dat0)

suppressWarnings(fit0 <- lavaan::sem(mod, dat0, missing = "fiml.x",
                                     group = "gp"))

fit0_data <- lav_data_used(fit0)
head(fit0_data)

exo_vars <- c("iv1", "iv2")
fit0_data_exo <- dat0[, exo_vars]
fit0_data_exo_g <- split(fit0_data_exo, dat0$gp)
em_tmp <- function(x) {
    suppressWarnings(em_out_fit <- lavaan::lavCor(x,
                                                  missing = "fiml",
                                                  output = "fit"))
    em_out <- list(param = list())
    tmp <- lavaan::lavInspect(em_out_fit, "implied")
    em_out$param$beta <- tmp$mean
    em_out$param$sigma <- tmp$cov
    em_out
  }
md_predictors_check0 <- lapply(fit0_data_exo_g,
                          function(x) {
                              em_out <- em_tmp(x)
                              out <- modi::MDmiss(x,
                                                  em_out$param$beta,
                                                  em_out$param$sigma)
                              list(out, em_out)
                            })
md_predictors_check <- unlist(lapply(md_predictors_check0, function(x) x[[1]]))

md_predictors <- mahalanobis_predictors(fit0)

test_that("Compare Mahalanobis distances: lavaan", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors)),
        sort(md_predictors_check)
      )
  })

j <- order(as.numeric(unlist((sapply(fit0_data_exo_g, rownames)))))
suppressWarnings(rerun_out <- lavaan_rerun(fit0, parallel = FALSE,
                                           to_rerun = 1:3))

md_predictors_rerun <- mahalanobis_predictors(rerun_out)

test_that("Compare Mahalanobis distances: lavaan_rerun", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors_rerun)),
        sort(md_predictors_check[j[1:3]])
      )
  })
