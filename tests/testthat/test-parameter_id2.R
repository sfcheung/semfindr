library(testthat)
library(lavaan)

dat <- sem_dat
set.seed(64264)
dat$gp <- sample(c("gp1", "gp2", "gp3"),
                 nrow(dat),
                 replace = TRUE)

sem_model_gp_eq <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~   a * f1
f3 ~   b * f2
ab := a*b
"

fit_gp_eq <- sem(sem_model_gp_eq, dat, group = "gp",
                 group.equal = "loadings")

partable <- parameterTable(fit_gp_eq)
est <- parameterestimates(fit_gp_eq)

# Expect error
test_that("Parameters not found", {
    expect_error(pars_id("f1 =~ x1", fit_gp_eq, where = "partable", free_only = TRUE))
    expect_error(pars_id("f1 =~ x1", fit_gp_eq, where = "coef", free_only = FALSE))
    expect_error(pars_id("f1 =~ x1", fit_gp_eq, where = "partable"))
  })

# Valid
test_that("Parameters not found", {
    expect_equal(pars_id(":=", fit_gp_eq, where = "partable"),
                 106)
    expect_equal(pars_id_label("ab", fit_gp_eq, where = "partable"),
                 106)
    expect_equal(pars_id_label("a", fit_gp_eq, where = "partable"),
                 c(10, 45, 80))
    expect_equal(pars_id_label("b", fit_gp_eq, where = "partable"),
                 c(11, 46, 81))
  })