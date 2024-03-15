skip_if_not_installed("MASS")
library(testthat)
library(lavaan)
library(semfindr)

mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

dat0 <- cfa_dat_heywood
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

suppressWarnings(fit <- lavaan::cfa(mod, dat0, group = "gp",
                                    group.equal = c("loadings")))

attr(lavaan_rerun_check(fit), "info")

test_that("Reject inadmissible solution", {
    expect_error(lavaan_rerun(fit))
  })

fit_rerun <- lavaan_rerun(fit, allow_inadmissible = TRUE,
                          to_rerun = c(1, 3, 5, 8))

test_that("Warnings", {
    expect_equal(sum(sapply(fit_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 3)
  })

dat2 <- dat0[-1, ]
fit2 <- lavaan::cfa(mod, dat2, group = "gp", group.equal = c("loadings"))
fit2_rerun <- lavaan_rerun(fit2, allow_inadmissible = TRUE,
                           to_rerun = c(3, 5, 8) + 1)

test_that("Warnings", {
    expect_equal(sum(sapply(fit2_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 0)
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
dat1[6, 1] <- -3
dat1[6, 2] <-  3
set.seed(856041)
dat1$gp <- sample(c("gp2", "gp1"), size = nrow(dat1), replace = TRUE)
cov(dat1[dat1$gp == "gp1", -4], use = "complete.obs")
cov(dat1[dat1$gp == "gp2", -4], use = "complete.obs")

cfa_dat_heywood <- dat1

mod <-
"
f1 =~ x1 + x2 + x3
"
suppressWarnings(fit <- lavaan::cfa(mod, cfa_dat_heywood, group = "gp"))

attr(lavaan_rerun_check(fit), "info")

test_that("Reject inadmissible solution", {
    expect_error(lavaan_rerun(fit))
  })

tmp <- c(3, 6, 5)
fit_rerun <- lavaan_rerun(fit, to_rerun = tmp, allow_inadmissible = TRUE)
suppressWarnings(fit1 <- lavaan::cfa(mod, cfa_dat_heywood[-3, ], group = "gp", se = "none"))
suppressWarnings(fit2 <- lavaan::cfa(mod, cfa_dat_heywood[-6, ], group = "gp", se = "none"))
suppressWarnings(fit3 <- lavaan::cfa(mod, cfa_dat_heywood[-5, ], group = "gp", se = "none"))
chk <- sum(sapply(list(fit1, fit2, fit3), function(x) {
                    inherits(tryCatch(lavInspect(x, "post.check"),
                            warning = function(w) w),
                            "simpleWarning")
                  }))

test_that("Warnings", {
    expect_equal(sum(sapply(fit_rerun$post_check, inherits,
                            what = "simpleWarning")),
                 chk)
  })
