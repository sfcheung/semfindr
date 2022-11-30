library(testthat)
library(lavaan)

dat <- sem_dat
set.seed(64264)
dat$gp <- sample(c("gp1", "gp2", "gp3"),
                 nrow(dat),
                 replace = TRUE)
sem_model <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~   f1
f3 ~   f2
"

sem_model_eq <-
"
f1 =~  x1 + a * x2 + b * x3
f2 =~  x4 + d * x5 + d * x6
f3 =~  x7 + x8 + x9
f2 ~   f1
f3 ~   f2
a == b
"

sem_model_gp_eq <-
"
f1 =~  x1 + c(a21, a22, a23) * x2 + x3
f2 =~  x4 + x5 + c(a61, a62, a63) * x6
f3 =~  x7 + x8 + x9
f2 ~   f1
f3 ~   f2
"

fit_ng <- sem(sem_model, dat)
fit_ng_eq <- sem(sem_model_eq, dat)
fit_gp <- sem(sem_model, dat, group = "gp")
fit_gp_eq <- sem(sem_model_gp_eq, dat, group = "gp")

fit_ng_eq@Model@eq.constraints
fit_gp_eq@Model@eq.constraints

pars1 <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
pars2 <- c("f1 =~ x2", "f2 =~ c(NA, 1, NA) * x5", "f2 ~ f1")
pars3 <- c("f1 =~ x2", "f2 =~ c(NA, 1, NA) * x5", "f2 ~ c(1, NA, 1) * f1")

# Test conditions
# Single-group model / Single-group specification
# Single-group model / Multi-group specification
# Single-group model with equality constraints / Single-group specification
# Single-group model with equality constraints / Multi-group specification
# Multi-group model with equality constraints / Single-group specification
# Multi-group model with equality constraints / Multi-group specification

pt_ng <- parameterTable(fit_ng)
pt_ng_eq <- parameterTable(fit_ng_eq)
pt_gp <- parameterTable(fit_gp)

pl_ng <- lavaan::lav_partable_labels(pt_ng, type = "user")
pl_ng_eq <- lavaan::lav_partable_labels(pt_ng_eq, type = "user")
pl_gp <- lavaan::lav_partable_labels(pt_gp, type = "user")
setdiff(names(coef(fit_ng)), pl_ng)
setdiff(names(coef(fit_ng_eq)), pl_ng_eq)
setdiff(names(coef(fit_gp)), pl_gp)
setdiff(pl_gp, names(coef(fit_gp)))

test_that("pars_id: default, where = 'coef'", {
    expect_true(all(pars_id(pars1, fit_ng) %in%
                    c(1, 3, 7)))
    expect_identical(pars_id(pars2, fit_ng), numeric(0))
    expect_identical(pars_id(pars3, fit_ng), numeric(0))
    expect_true(all(pars_id(pars1, fit_ng_eq) %in%
                    c(1, 3, 7)))
    expect_identical(pars_id(pars2, fit_ng_eq), numeric(0))
    expect_identical(pars_id(pars3, fit_ng_eq), numeric(0))
    expect_true(all(pars_id(pars1, fit_gp) %in%
                    c(1, 3, 7, 30, 32, 36, 59, 61, 65)))
    expect_true(all(pars_id(pars2, fit_gp) %in%
                    c(1, 3, 7, 30, 36, 59, 61, 65)))
    expect_true(all(pars_id(pars3, fit_gp) %in%
                    c(1, 3, 30, 36, 59, 61)))
  })


test_that("pars_id: where = 'partable'", {
    expect_true(all(pars_id(pars1, fit_ng, where = "partable") %in%
                    which(pt_ng$free %in% c(1, 3, 7))))
    expect_identical(pars_id(pars2, fit_ng, where = "partable"), numeric(0))
    expect_identical(pars_id(pars3, fit_ng, where = "partable"), numeric(0))
    expect_true(all(pars_id(pars1, fit_ng_eq, where = "partable")
                    %in% which(pt_ng_eq$free %in% c(1, 3, 7))))
    expect_identical(pars_id(pars2, fit_ng_eq, where = "partable"), numeric(0))
    expect_identical(pars_id(pars3, fit_ng_eq, where = "partable"), numeric(0))
    expect_true(all(pars_id(pars1, fit_gp, where = "partable") %in%
                    which(pt_gp$free %in% c(1, 3, 7, 30, 32, 36, 59, 61, 65))))
    expect_true(all(pars_id(pars2, fit_gp, where = "partable") %in%
                    which(pt_gp$free %in% c(1, 3, 7, 30, 36, 59, 61, 65))))
    expect_true(all(pars_id(pars3, fit_gp, where = "partable") %in%
                    which(pt_gp$free %in% c(1, 3, 30, 36, 59, 61))))
  })

# pars_id_lor

pars1 <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
pars2 <- c("f1 =~ x2", "f2 =~ x5.gp2", "f2 ~ f1", "f2 =~ x5.gp3")
pars3 <- c("f1 =~ x2", "f2 =~ x5.gp2", "f2 =~ x5.gp3", "f2 ~ f1.gp1")
