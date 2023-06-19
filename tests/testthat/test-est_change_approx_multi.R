library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:40, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit <- lavaan::sem(mod, dat0, group = "gp")

# From scores
fit_est_change_approx <- lavScores(fit,
                                   ignore.constraints = TRUE,
                                   remove.duplicated = FALSE) %*% vcov(fit) *
  nobs(fit) / (nobs(fit) - 1)
# Use vcov directly
# # Hessian (inverse of covariance) with scale adjustment
# information_fit <- lavInspect(fit, what = "information") * (nobs(fit) - 1)
# # Compare information_fit with vcov
# tmp1 <- solve(lavTech(fit, what = "information") * (nobs(fit)))
# tmp2 <- lavTech(fit, "vcov")
# # Short cut for computing quadratic form (https://stackoverflow.com/questions/27157127/efficient-way-of-calculating-quadratic-forms-avoid-for-loops)
# gcd_approx <- rowSums(
#   (fit_est_change_approx %*% information_fit) * fit_est_change_approx
# )
tmp2 <- lavTech(fit, "vcov")
tmp3 <- full_rank(tmp2)
i <- tmp3$dropped
n <- nobs(fit)
gcd_approx <- rowSums(
  (fit_est_change_approx[, -i] %*% solve(tmp3$final * n) * (n - 1)) * fit_est_change_approx[, -i]
)

gcd_approx2 <- est_change_approx(fit)

test_that("Check against known results", {
    expect_equal(ignore_attr = TRUE,
        gcd_approx2[, "gcd_approx"],
        gcd_approx
      )
  })

test1 <- est_change_approx(fit, c("~"))
test2 <- est_change_approx(fit, c("~~"))
test3 <- est_change_approx(fit, c("m1 ~ iv1", "~~"))

test_that("est_change_raw_approx: Selected parameters", {
    expect_equal(setdiff(colnames(test1),
                  c("m1~iv1", "m1~iv2", "dv~m1",
                    "m1~iv1.g2", "m1~iv2.g2", "dv~m1.g2",
                    "gcd_approx")),
                 character(0))
    expect_equal(setdiff(colnames(test2),
                  c("iv1~~iv2", "m1~~m1", "dv~~dv",
                    "iv1~~iv1", "iv2~~iv2",
                    "iv1~~iv2.g2", "m1~~m1.g2", "dv~~dv.g2",
                    "iv1~~iv1.g2", "iv2~~iv2.g2",
                    "gcd_approx")),
                 character(0))
    expect_equal(setdiff(colnames(test3),
                  c("iv1~~iv2", "m1~iv1", "m1~~m1",
                    "dv~~dv", "iv1~~iv1", "iv2~~iv2",
                    "iv1~~iv2.g2", "m1~iv1.g2", "m1~~m1.g2",
                    "dv~~dv.g2", "iv1~~iv1.g2", "iv2~~iv2.g2",
                    "gcd_approx")),
                 character(0))
  })

test_that("User parameters should return error or excluded", {
    expect_error(est_change_approx(fit, "a2b"))
    expect_equal(intersect(colnames(est_change_approx(fit, c("m1 ~ iv1", "a1b"))), "a2b"),
                 character(0))
  })


# With fixed parameters

mod <-
'
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit <- lavaan::sem(mod, dat0, group = "gp")

# From scores
fit_est_change_approx <- lavScores(fit,
                                   ignore.constraints = TRUE,
                                   remove.duplicated = FALSE) %*% vcov(fit) *
  nobs(fit) / (nobs(fit) - 1)
# Use vcov directly
# # Hessian (inverse of covariance) with scale adjustment
# information_fit <- lavInspect(fit, what = "information") * (nobs(fit) - 1)
# # Compare information_fit with vcov
# tmp1 <- solve(lavTech(fit, what = "information") * (nobs(fit)))
# tmp2 <- lavTech(fit, "vcov")
# # Short cut for computing quadratic form (https://stackoverflow.com/questions/27157127/efficient-way-of-calculating-quadratic-forms-avoid-for-loops)
# gcd_approx <- rowSums(
#   (fit_est_change_approx %*% information_fit) * fit_est_change_approx
# )
tmp2 <- lavTech(fit, "vcov")
tmp3 <- full_rank(tmp2)
i <- tmp3$dropped
n <- nobs(fit)
gcd_approx <- rowSums(
  (fit_est_change_approx[, -i] %*% solve(tmp3$final * n) * (n - 1)) * fit_est_change_approx[, -i]
)

gcd_approx2 <- est_change_approx(fit)

test_that("Check against known results", {
    expect_equal(ignore_attr = TRUE,
        gcd_approx2[, "gcd_approx"],
        gcd_approx
      )
  })


# CFA model with selected loadings

mod <-
'
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f1 ~~ f2
'

dat <- cfa_dat

dat0 <- dat[1:100, ]
set.seed(85604)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit <- lavaan::cfa(mod, dat0, group = "gp", group.equal = "loadings")

# From scores
# fit_est_change_approx <- lavScores(fit) %*% vcov(fit) *
#   nobs(fit) / (nobs(fit) - 1)
# fit_est_change_approx <- fit_est_change_approx[, 1:4]
# # Hessian (inverse of covariance) with scale adjustment
# information_fit <- lavInspect(fit, what = "information") * (nobs(fit) - 1)
# information_fit <- information_fit[1:4, 1:4]
# # Compare information_fit with vcov
# tmp1 <- solve(lavTech(fit, what = "information") * (nobs(fit)))
# tmp2 <- lavTech(fit, "vcov")
# tmp1 <- tmp1[1:4, 1:4]
# tmp2 <- tmp2[1:4, 1:4]
# # Short cut for computing quadratic form (https://stackoverflow.com/questions/27157127/efficient-way-of-calculating-quadratic-forms-avoid-for-loops)
# gcd_approx <- rowSums(
#   (fit_est_change_approx %*% information_fit) * fit_est_change_approx
# )
# Use vcov directly
fit_est_change_approx <- lavScores(fit,
                                   ignore.constraints = TRUE,
                                   remove.duplicated = FALSE) %*% vcov(fit) *
  nobs(fit) / (nobs(fit) - 1)
fit_est_change_approx <- fit_est_change_approx[, c(1:4, 20:23)]
tmp2 <- lavTech(fit, "vcov")[c(1:4, 20:23), c(1:4, 20:23)]
tmp3 <- full_rank(tmp2)
i <- tmp3$dropped
n <- nobs(fit)
gcd_approx <- rowSums(
  (fit_est_change_approx[, -i] %*% solve(tmp3$final * n) * (n - 1)) * fit_est_change_approx[, -i]
)

gcd_approx2 <- est_change_approx(fit, parameters = "=~")

test_that("Check against known results", {
    expect_equal(ignore_attr = TRUE,
        gcd_approx2[, "gcd_approx"],
        gcd_approx
      )
  })

