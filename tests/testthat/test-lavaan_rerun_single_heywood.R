skip_if_not_installed("MASS")
library(testthat)
library(lavaan)
library(semfindr)

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

suppressWarnings(fit <- lavaan::cfa(mod, cfa_dat_heywood))

attr(lavaan_rerun_check(fit), "info")

test_that("Reject inadmissible solution", {
    skip_if(suppressWarnings(lavInspect(fit, "post.check")))
    expect_error(lavaan_rerun(fit))
  })

# Disable these tests. Highly dependent on lavaan versions

# fit_rerun <- lavaan_rerun(fit, allow_inadmissible = TRUE)

# test_that("Warnings", {
#     expect_equal(sum(sapply(fit_rerun$post_check, inherits,
#                             what = "simpleWarning")),
#                  26)
#   })

# dat2 <- cfa_dat_heywood[-1, ]
# fit2 <- lavaan::cfa(mod, dat2)
# fit2_rerun <- lavaan_rerun(fit2, allow_inadmissible = TRUE)

# test_that("Warnings", {
#     expect_equal(sum(sapply(fit2_rerun$post_check, inherits,
#                             what = "simpleWarning")),
#                  0)
#   })

# With Listwise

n <- 20
sigma <- matrix(.3, 3, 3)
diag(sigma) <- 1
sigma <- sigma * (n - 1) / n
set.seed(1234)
dat0 <- MASS::mvrnorm(n, rep(0, 3), sigma, empirical = TRUE)
dat0 <- as.data.frame(dat0)
colnames(dat0) <- paste0("x", 1:3)
cov(dat0)

dat0[1, 2] <- dat0[2, 3] <- dat0[4, ] <- NA
dat0[6, 1] <- -10
dat0[6, 2] <-  10
cov(dat0, use = "complete.obs")
cfa_dat_heywood <- dat0

mod <-
"
f1 =~ x1 + x2 + x3
"
suppressWarnings(fit <- lavaan::cfa(mod, cfa_dat_heywood))

attr(lavaan_rerun_check(fit), "info")

test_that("Reject inadmissible solution", {
    skip_if(suppressWarnings(lavInspect(fit, "post.check")))
    expect_error(lavaan_rerun(fit))
  })

# Disable these tests. Highly dependent on lavaan versions

# fit_rerun <- lavaan_rerun(fit, allow_inadmissible = TRUE)

# test_that("Warnings", {
#     expect_equal(sum(sapply(fit_rerun$post_check, inherits,
#                             what = "simpleWarning")),
#                  14)
#   })
