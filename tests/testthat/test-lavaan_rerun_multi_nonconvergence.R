skip_on_cran()
# "Essential but may be machine dependent"

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
