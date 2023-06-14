library(testthat)
library(lavaan)
library(semfindr)

#context("Test implied_scores with x variables free")

mod <-
'
iv1 ~~ iv2
m1 ~ c(a1, a2) * iv1 + iv2
dv ~ c(b, b) * m1
a1b := a1*b
a2b := a2*b
'

dat <- pa_dat
dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)
head(dat0)
fit0 <- lavaan::sem(mod, dat0, meanstructure = TRUE, group = "gp")
group_label <- lavInspect(fit0, "group.label")

implied_scores0 <- implied_scores(fit0)
implied_scores0_g <- split(as.data.frame(implied_scores0), dat0$gp)
implied_scores0_g <- implied_scores0_g[group_label]
fitted0 <- lavInspect(fit0, "fitted")
est <- parameterEstimates(fit0)

y_names <- c("m1", "dv")
implied_means0 <- lapply(implied_scores0_g, colMeans)
implied_means_lavaan <- lapply(fitted0, function(x) x$mean[y_names])
implied_vars0 <- lapply(implied_scores0_g, function(x) apply(x, 2, var))
y_evar0 <- est[(est$op == "~~") & (est$lhs %in% y_names) & (est$rhs %in% y_names), ]
y_evar1 <- setNames(y_evar0$est, y_evar0$lhs)
y_evar <- split(y_evar1, y_evar0$group)
implied_vars1 <- mapply(function(x, y) {x + y}, x = implied_vars0, y = y_evar,
                        SIMPLIFY = FALSE)
implied_vars_lavaan <- lapply(fitted0, function(x) diag(x$cov[y_names, y_names]))

test_that("Can implied means be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        implied_means0, implied_means_lavaan,
        tolerance = .00001
      )
  })

test_that("Can implied variances be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        implied_vars1, implied_vars_lavaan,
        tolerance = .01
      )
  })

#context("Test implied_scores with x variables fixed")

mod <-
'
m1 ~ c(a1, a2) * iv1 + iv2
dv ~ c(b, b) * m1
a1b := a1*b
a2b := a2*b
'

dat <- pa_dat
dat0 <- dat[1:100, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)
head(dat0)
fit0 <- lavaan::sem(mod, dat0, meanstructure = TRUE, group = "gp")
group_label <- lavInspect(fit0, "group.label")

implied_scores0 <- implied_scores(fit0)
implied_scores0_g <- split(as.data.frame(implied_scores0), dat0$gp)
implied_scores0_g <- implied_scores0_g[group_label]
fitted0 <- lavInspect(fit0, "fitted")
est <- parameterEstimates(fit0)

y_names <- c("m1", "dv")
implied_means0 <- lapply(implied_scores0_g, colMeans)
implied_means_lavaan <- lapply(fitted0, function(x) x$mean[y_names])
implied_vars0 <- lapply(implied_scores0_g, function(x) apply(x, 2, var))
y_evar0 <- est[(est$op == "~~") & (est$lhs %in% y_names) & (est$rhs %in% y_names), ]
y_evar1 <- setNames(y_evar0$est, y_evar0$lhs)
y_evar <- split(y_evar1, y_evar0$group)
implied_vars1 <- mapply(function(x, y) {x + y}, x = implied_vars0, y = y_evar,
                        SIMPLIFY = FALSE)
implied_vars_lavaan <- lapply(fitted0, function(x) diag(x$cov[y_names, y_names]))

test_that("Can implied means be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        implied_means0, implied_means_lavaan,
        tolerance = .00001
      )
  })

test_that("Can implied variances be reproduced?", {
    expect_equal(ignore_attr = TRUE,
        implied_vars1, implied_vars_lavaan,
        tolerance = .01
      )
  })
