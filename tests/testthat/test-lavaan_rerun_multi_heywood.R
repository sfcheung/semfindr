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
