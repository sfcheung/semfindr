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
# set.seed(2468)
# dput(sample.int(100, 40))
i <- c(21L, 22L, 16L, 72L, 13L, 50L, 39L, 91L, 66L, 100L, 62L, 26L,
17L, 35L, 48L, 81L, 55L, 97L, 98L, 3L, 59L, 89L, 77L, 84L, 4L,
20L, 41L, 43L, 49L, 90L, 34L, 47L, 71L, 14L, 52L, 5L, 80L, 31L,
2L, 53L)
dat <- cfa_dat[i, ]

fit <- lavaan::cfa(mod, dat, control = list(iter.max = 45))
lavInspect(fit, "iterations")

# This rerun has both runs that failed to converge and runs that failed post.check.
j <- c(1, c(2, 16, 20, 21, 22, 24), 25)
fit_rerun <- lavaan_rerun(fit, to_rerun = j, skip_all_checks = TRUE)
table(sapply(fit_rerun$rerun, lavInspect, what = "converged"))
suppressWarnings(table(sapply(fit_rerun$rerun, lavInspect, what = "post.check")))

tmp <- sapply(j, function(x) suppressWarnings(lavaan::cfa(mod, dat[-x, ], control = list(iter.max = 45))))
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

datm <- dat[1:30, ]
datm[1, 2] <- datm[2, 3] <- datm[3, 4] <- datm[5, ] <- NA
fitm <- lavaan::cfa(mod, datm, control = list(iter.max = 40))
lavInspect(fitm, "iterations")
# rerun_out <- lavaan_rerun(fitm, parallel = FALSE)

# This rerun has both runs that failed to converge and runs that failed post.check.
j <- c(16, 17, 20, 30)
fit_rerun <- lavaan_rerun(fitm, to_rerun = j, skip_all_checks = TRUE)
# table(sapply(fit_rerun$rerun, lavInspect, what = "converged"))
# suppressWarnings(table(sapply(fit_rerun$rerun, lavInspect, what = "post.check")))

tmp <- sapply(j, function(x) suppressWarnings(lavaan::cfa(mod, datm[-x, ], control = list(iter.max = 40))))
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
