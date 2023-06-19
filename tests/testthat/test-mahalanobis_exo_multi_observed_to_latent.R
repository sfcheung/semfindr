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

dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")

rerun_out <- lavaan_rerun(fit0, parallel = FALSE, to_rerun = 1:3)

fit0_data <- lav_data_used(fit0)
exo_vars <- c("x4", "x5")
fit0_data_exo <- dat0[, exo_vars]
fit0_data_exo_g <- split(fit0_data_exo, dat0$gp)
md_predictors_check <- lapply(fit0_data_exo_g,
                          function(x) {
                              mahalanobis(x, colMeans(x), cov(x))
                            })
md_predictors_check <- unlist(md_predictors_check, use.names = FALSE)

md_predictors <- mahalanobis_predictors(fit0)

md_predictors_rerun <- mahalanobis_predictors(rerun_out)

j <- order(as.numeric(unlist((sapply(fit0_data_exo_g, rownames)))))

test_that("Compare Mahalanobis distances: lavaan_rerun", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors)),
        sort(md_predictors_check)
      )
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_predictors_rerun)),
        sort(md_predictors_check[j[1:3]])
      )
  })

