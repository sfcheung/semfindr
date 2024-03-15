skip_on_cran()
# "Essential but may be machine dependent"
skip_if_not_installed("MASS")

library(testthat)
library(lavaan)
library(semfindr)

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'
dat <- cfa_dat[1:60, ]
# set.seed(123456)
# dat$gp <- sample(c("gp2", "gp1"), size = nrow(dat), replace = TRUE)
dat$gp <- c("gp1", "gp1", "gp1", "gp2", "gp1", "gp2", "gp2", "gp1", "gp1",
"gp1", "gp1", "gp2", "gp2", "gp1", "gp2", "gp1", "gp2", "gp2",
"gp2", "gp2", "gp1", "gp1", "gp1", "gp1", "gp1", "gp2", "gp2",
"gp1", "gp1", "gp2", "gp1", "gp1", "gp1", "gp2", "gp2", "gp1",
"gp2", "gp2", "gp1", "gp1", "gp1", "gp2", "gp2", "gp1", "gp2",
"gp1", "gp2", "gp1", "gp2", "gp1", "gp1", "gp1", "gp1", "gp1",
"gp2", "gp1", "gp1", "gp2", "gp2", "gp1")

fit <- lavaan::cfa(mod, dat, control = list(iter.max = 53),
                   group = "gp", group.equal = "loadings")
lavInspect(fit, "iterations")
lavInspect(fit, "post.check")

# This rerun has both runs that failed to converge and runs that failed post.check.
j <- 8:13
fit_rerun <- lavaan_rerun(fit, to_rerun = j, skip_all_checks = TRUE)
table(sapply(fit_rerun$rerun, lavInspect, what = "converged"))
suppressWarnings(table(sapply(fit_rerun$rerun, lavInspect, what = "post.check")))

tmp <- sapply(j, function(x) suppressWarnings(lavaan::cfa(mod, dat[-x, ], control = list(iter.max = 53),
                                              group = "gp", group.equal = "loadings")))
nc_check0 <- sapply(tmp, lavInspect, what = "converged")
nc_check1 <- sum(nc_check0)
pc_check0 <- suppressWarnings(sapply(tmp, lavInspect, what = "post.check"))
pc_check1 <- sum(!pc_check0)

test_that("Convergence", {
    expect_equal(sum(sapply(fit_rerun$rerun, lavInspect,
                            what = "converged")),
                 nc_check1)
    expect_equal(fit_rerun$converged,
                 nc_check0,
                 ignore_attr = TRUE)
  })

test_that("Warnings", {
    expect_equal(sum(sapply(fit_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 pc_check1)
    expect_equal(which(sapply(fit_rerun$post_check, inherits,
                      what = "simpleWarning")),
                 which(!pc_check0),
                 ignore_attr = TRUE)
  })

test_that("est_change_raw", {
    tmp <- est_change_raw(fit_rerun)
    expect_equal(complete.cases(tmp),
                 nc_check0)
  })

test_that("est_change", {
    tmp <- est_change(fit_rerun)
    expect_equal(complete.cases(tmp),
                 nc_check0)
  })

test_that("fit_measures_change", {
    tmp <- fit_measures_change(fit_rerun, fit_measures = "chisq")
    expect_equal(as.vector(!is.na(tmp)),
                 nc_check0)
  })


# With Listwise

n <- 50
sigma <- matrix(.3, 3, 3)
diag(sigma) <- 1
sigma <- sigma * (n - 1) / n
set.seed(12345)
dat0 <- MASS::mvrnorm(n, rep(0, 3), sigma, empirical = TRUE)
dat0 <- as.data.frame(dat0)
colnames(dat0) <- paste0("x", 1:3)
cov(dat0)

dat1 <- dat0
dat1[1, 2] <- dat1[2, 3] <- dat1[4, ] <- NA
dat1[6, 1] <- -10
dat1[6, 2] <-  10
set.seed(856041)
dat1$gp <- sample(c("gp2", "gp1"), size = nrow(dat1), replace = TRUE)
cor(dat1[dat1$gp == "gp1", -4], use = "complete.obs")
cor(dat1[dat1$gp == "gp2", -4], use = "complete.obs")

cfa_dat_heywood <- dat1

mod <-
"
f1 =~ x1 + x2 + x3
"
suppressWarnings(fit <- lavaan::cfa(mod, cfa_dat_heywood, group = "gp", control = list(iter.max = 10)))

attr(lavaan_rerun_check(fit), "info")

test_that("Reject inadmissible solution", {
    expect_error(lavaan_rerun(fit))
  })

j <- c(3, 7, 5)
fit_rerun <- lavaan_rerun(fit, to_rerun = j, skip_all_checks = TRUE)
tmp <- sapply(j, function(x) {suppressWarnings(lavaan::cfa(mod, dat[-x, ], control = list(iter.max = 10), group = "gp"))})
nc_check0 <- sapply(tmp, lavInspect, what = "converged")
nc_check1 <- sum(nc_check0)
pc_check0 <- suppressWarnings(sapply(tmp, lavInspect, what = "post.check"))
pc_check1 <- sum(!pc_check0)

test_that("Convergence", {
    expect_equal(sum(sapply(fit_rerun$rerun, lavInspect,
                            what = "converged")),
                 nc_check1)
    expect_equal(fit_rerun$converged,
                 nc_check0,
                 ignore_attr = TRUE)
  })
