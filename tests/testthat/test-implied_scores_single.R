library(testthat)
library(lavaan)
library(semfindr)

#context("Test implied_scores with x variables free")

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat
y_names <- c("m1", "dv")
fit0 <- lavaan::sem(mod, dat, meanstructure = TRUE)
implied_scores0 <- implied_scores(fit0)
implied_means0 <- colMeans(implied_scores0)
implied_means_lavaan <- fitted(fit0)$mean[y_names]
class(implied_means_lavaan) <- "numeric"

fit_rsquare_raw <- lavaan::parameterEstimates(fit0, rsquare = TRUE)
fit_rsquare_raw <- fit_rsquare_raw[fit_rsquare_raw$op == "r2", ]
fit_rsquare <- fit_rsquare_raw$est
names(fit_rsquare) <- fit_rsquare_raw$lhs

dat_y <- dat[, y_names]
p <- length(y_names)
y_hat_rsquare <- rep(NA, p)
names(y_hat_rsquare) <- y_names
for (i in y_names) {
    y_hat_rsquare[i] <-
        suppressWarnings(cor(dat[, i], implied_scores0[, i], use = "pairwise.complete.obs")^2)
}
y_hat_rsquare <- y_hat_rsquare[!is.na(y_hat_rsquare)]

test_that("Can implied means be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        implied_means0, implied_means_lavaan,
        tolerance = .00001
      )
  })

test_that("Can implied R-squares be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        y_hat_rsquare, fit_rsquare,
        tolerance = .00001
      )
  })

#context("Test implied_scores with x variables fixed")

mod <-
'
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat
y_names <- c("m1", "dv")
fit0 <- lavaan::sem(mod, dat, meanstructure = TRUE, fixed.x = TRUE)
implied_scores0 <- implied_scores(fit0)
implied_means0 <- colMeans(implied_scores0)
implied_means_lavaan <- fitted(fit0)$mean[y_names]
class(implied_means_lavaan) <- "numeric"

fit_rsquare_raw <- lavaan::parameterEstimates(fit0, rsquare = TRUE)
fit_rsquare_raw <- fit_rsquare_raw[fit_rsquare_raw$op == "r2", ]
fit_rsquare <- fit_rsquare_raw$est
names(fit_rsquare) <- fit_rsquare_raw$lhs

dat_y <- dat[, y_names]
p <- length(y_names)
y_hat_rsquare <- rep(NA, p)
names(y_hat_rsquare) <- y_names
for (i in y_names) {
    y_hat_rsquare[i] <-
        suppressWarnings(cor(dat[, i], implied_scores0[, i], use = "pairwise.complete.obs")^2)
}
y_hat_rsquare <- y_hat_rsquare[!is.na(y_hat_rsquare)]

test_that("Can implied means be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        implied_means0, implied_means_lavaan,
        tolerance = .00001
      )
  })

test_that("Can implied R-squares be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        y_hat_rsquare, fit_rsquare,
        tolerance = .00001
      )
  })

# dat <- pa_dat
# dat[1, 2] <- NA # Impute one missing data
# y_names <- c("m1", "dv")
# fit0 <- lavaan::sem(mod, dat, meanstructure = TRUE, missing = "fiml")

# test_that("Can detect a dataset with missing data", {
#     expect_error(implied_scores(fit0), "missing data")
#   })

