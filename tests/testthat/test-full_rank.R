library(testthat)
library(lavaan)
library(semfindr)

dat <- pa_dat2
mod <-
"
m1 ~ a1 * iv1 + a2 * iv2
dv ~ b * m1
a1 == a2
"
fit <- sem(mod, pa_dat2, fixed.x = FALSE)
summary(fit)

x <- vcov(fit)
x_full <- full_rank(x)

test_that("full_rank: Simple", {
    expect_error(solve(x_full$original))
    expect_equal(as.numeric(Matrix::rankMatrix(x_full$final)),
                 7)
  })

data(cfa_dat)
mod <-
"
f1 =~  x1 + a1 * x2 + a2 * x3
f2 =~  x4 + a3 * x5 + a4 * x6
a12 := a1 + a2
a34 := a3 + a4
a12 == a34
"
fit2 <- cfa(mod, cfa_dat)

x <- vcov(fit2)
x_full <- full_rank(x)
x2 <- x
x2[c(1, 2), ] <- x[c(2, 1), ]
x2[, c(1, 2)] <- x[, c(2, 1)]
x2_full <- full_rank(x2)

test_that("full_rank: Simple", {
    expect_error(solve(x_full$original))
    expect_equal(as.numeric(Matrix::rankMatrix(x_full$final)),
                 12)
    expect_equal(x_full$dropped,
                 1)
    expect_error(solve(x2_full$original))
    expect_equal(as.numeric(Matrix::rankMatrix(x2_full$final)),
                 12)
    expect_equal(x2_full$dropped,
                 1)
  })

dat <- pa_dat2
mod <-
"
m1 ~ a1 * iv1 + a2 * iv2
dv ~ b * m1
a1 == a2
a2 == b
"
fit <- sem(mod, pa_dat2, fixed.x = FALSE)
summary(fit)

x <- vcov(fit)
x_full <- full_rank(x)

test_that("full_rank: Simple", {
    expect_error(solve(x_full$original))
    expect_equal(as.numeric(Matrix::rankMatrix(x_full$final)),
                 6)
  })
