library(testthat)
library(lavaan)
library(semfindr)

#context("Test fit measure changes")

mod <-
'
iv1 ~~ iv2
m1 ~ c(a1, a2) * iv1 + c(NA, NA) * iv2
dv ~ c(b, b) * m1
'

dat <- pa_dat
dat0 <- dat[1:60, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit <- lavaan::sem(mod, dat0, group = "gp")

fit_measures_change_approx2 <- fit_measures_change_approx(fit)

lli <- lavInspect(fit, what = "loglik.casewise", drop.list.single.group = FALSE)
lli <- unlist(lli, use.names = FALSE)
mod_h1 <- lav_partable_unrestricted(fit)
fit_h1 <- sem(mod_h1, dat0, group = "gp")
lli_h1 <- lavInspect(fit_h1, what = "loglik.casewise", drop.list.single.group = FALSE)
lli_h1 <- unlist(lli_h1)

chisq_change_i_approx <- as.vector(2 * (lli_h1 - lli))

# Need to add tests for CFI and TLI
test_that("Check against known results", {
    expect_equal(ignore_attr = TRUE,
        sort(fit_measures_change_approx2[, "chisq"]),
        sort(chisq_change_i_approx)
      )
  })

