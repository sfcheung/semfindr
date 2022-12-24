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
pt_gp_labelled <- pt_gp
pt_gp_labelled$group <- lavInspect(fit_gp, "group.label")[pt_gp_labelled$group]


est_ng <- parameterEstimates(fit_ng, standardized = TRUE)
est_ng_eq <- parameterEstimates(fit_ng_eq, standardized = TRUE)
est_gp <- parameterEstimates(fit_gp, standardized = TRUE)

pl_ng <- lavaan::lav_partable_labels(pt_ng, type = "user")
pl_ng_eq <- lavaan::lav_partable_labels(pt_ng_eq, type = "user")
pl_gp <- lavaan::lav_partable_labels(pt_gp, type = "user")
setdiff(names(coef(fit_ng)), pl_ng)
setdiff(names(coef(fit_ng_eq)), pl_ng_eq)
setdiff(names(coef(fit_gp)), pl_gp)
setdiff(pl_gp, names(coef(fit_gp)))

pars1 <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
pars2 <- c("f1 =~ x2", "f2 =~ c(NA, 1, NA) * x5", "f2 ~ f1")
pars3 <- c("f1 =~ x2", "f2 =~ c(NA, 1, NA) * x5", "f2 ~ c(1, NA, 1) * f1")

test_that("pars_id: default, where = 'coef'", {
    expect_true(setequal(pars_id(pars1, fit_ng),
                    c(1, 3, 7)))
    expect_true(setequal(pars_id(pars2, fit_ng),
                    c(1, 7)))
    expect_true(setequal(pars_id(pars3, fit_ng),
                    c(1)))
    expect_true(setequal(pars_id(pars1, fit_ng_eq),
                    c(1, 3, 7)))
    expect_true(setequal(pars_id(pars2, fit_ng_eq),
                    c(1, 7)))
    expect_true(setequal(pars_id(pars3, fit_ng_eq),
                    c(1)))
    expect_true(setequal(pars_id(pars1, fit_gp),
                    c(1, 3, 7, 30, 32, 36, 59, 61, 65)))
    expect_true(setequal(pars_id(pars2, fit_gp),
                    c(1, 3, 7, 30, 36, 59, 61, 65)))
    expect_true(setequal(pars_id(pars3, fit_gp),
                    c(1, 3, 30, 36, 59, 61)))
  })


test_that("pars_id: where = 'partable'", {
    expect_true(setequal(pars_id(pars1, fit_ng, where = "partable"),
                    which(pt_ng$free %in% c(1, 3, 7))))
    expect_true(setequal(pars_id(pars2, fit_ng, where = "partable"),
                    which(pt_ng$free %in% c(1, 7))))
    expect_true(setequal(pars_id(pars3, fit_ng, where = "partable"),
                    which(pt_ng$free %in% c(1))))
    expect_true(setequal(pars_id(pars1, fit_ng_eq, where = "partable"),
                    which(pt_ng_eq$free %in% c(1, 3, 7))))
    expect_true(setequal(pars_id(pars2, fit_ng_eq, where = "partable"),
                    which(pt_ng_eq$free %in% c(1, 7))))
    expect_true(setequal(pars_id(pars3, fit_ng_eq, where = "partable"),
                    which(pt_ng_eq$free %in% c(1))))
    expect_true(setequal(pars_id(pars1, fit_gp, where = "partable"),
                    which(pt_gp$free %in% c(1, 3, 7, 30, 32, 36, 59, 61, 65))))
    expect_true(setequal(pars_id(pars2, fit_gp, where = "partable"),
                    which(pt_gp$free %in% c(1, 3, 7, 30, 36, 59, 61, 65))))
    expect_true(setequal(pars_id(pars3, fit_gp, where = "partable"),
                    which(pt_gp$free %in% c(1, 3, 30, 36, 59, 61))))
  })

# pars_id_lorg

pars1 <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
pars2 <- c("f1 =~ x2", "f2 =~ x5.gp2", "f2 ~ f1", "f2 =~ x5.gp3")
pars3 <- c("f1 =~ x2", "f2 =~ x5.gp2", "f2 =~ x5.gp3", "f2 ~ f1.gp1")

test_that("pars_id_lorg: default, where = 'coef'", {
    expect_true(setequal(pars_id_lorg(pars1, fit_ng),
                    c(1, 3, 7)))
    expect_true(setequal(pars_id_lorg(pars2, fit_ng),
                    c(1, 7)))
    expect_true(setequal(pars_id_lorg(pars3, fit_ng),
                    c(1)))
    expect_true(setequal(pars_id_lorg(pars1, fit_ng_eq),
                    c(1, 3, 7)))
    expect_true(setequal(pars_id_lorg(pars2, fit_ng_eq),
                    c(1, 7)))
    expect_true(setequal(pars_id_lorg(pars3, fit_ng_eq),
                    c(1)))
    expect_true(setequal(pars_id_lorg(pars1, fit_gp),
                    c(1, 3, 7, 30, 32, 36, 59, 61, 65)))
    expect_true(setequal(pars_id_lorg(pars2, fit_gp),
                    c(1, 3, 7, 30, 36, 59, 61, 65)))
    expect_true(setequal(pars_id_lorg(pars3, fit_gp),
                    c(1, 3, 30, 36, 59, 61)))
  })

test_that("pars_id_lorg: where = 'partable'", {
    expect_true(setequal(pars_id_lorg(pars1, fit_ng, where = "partable"),
                    which(pt_ng$free %in% c(1, 3, 7))))
    expect_true(setequal(pars_id_lorg(pars2, fit_ng, where = "partable"),
                    which(pt_ng$free %in% c(1, 7))))
    expect_true(setequal(pars_id_lorg(pars3, fit_ng, where = "partable"),
                    which(pt_ng$free %in% c(1))))
    expect_true(setequal(pars_id_lorg(pars1, fit_ng_eq, where = "partable"),
                    which(pt_ng_eq$free %in% c(1, 3, 7))))
    expect_true(setequal(pars_id_lorg(pars2, fit_ng_eq, where = "partable"),
                    which(pt_ng_eq$free %in% c(1, 7))))
    expect_true(setequal(pars_id_lorg(pars3, fit_ng_eq, where = "partable"),
                    which(pt_ng_eq$free %in% c(1))))
    expect_true(setequal(pars_id_lorg(pars1, fit_gp, where = "partable"),
                    which(pt_gp$free %in% c(1, 3, 7, 30, 32, 36, 59, 61, 65))))
    expect_true(setequal(pars_id_lorg(pars2, fit_gp, where = "partable"),
                    which(pt_gp$free %in% c(1, 3, 7, 30, 36, 59, 61, 65))))
    expect_true(setequal(pars_id_lorg(pars3, fit_gp, where = "partable"),
                    which(pt_gp$free %in% c(1, 3, 30, 36, 59, 61))))
  })

test_that("pars_id_lorg and pars_id_to_lorg: where = 'partable'", {
    tmp1 <- pars_id_lorg(pars1, fit_ng, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_ng, type = "all")
    expect_true(all(paste0(tmp2$lhs, tmp2$op, tmp2$rhs) %in%
                    gsub(" ", "", pars1)))
    tmp1 <- pars_id_lorg(pars2, fit_ng, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_ng, type = "all")
    expect_true(all(paste0(tmp2$lhs, tmp2$op, tmp2$rhs) %in%
                    gsub(" ", "", pars2)))
    tmp1 <- pars_id_lorg(pars3, fit_ng, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_ng, type = "all")
    expect_true(all(paste0(tmp2$lhs, tmp2$op, tmp2$rhs) %in%
                    gsub(" ", "", pars3)))

    tmp1 <- pars_id_lorg(pars1, fit_ng_eq, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_ng_eq, type = "all")
    expect_true(all(paste0(tmp2$lhs, tmp2$op, tmp2$rhs) %in%
                    gsub(" ", "", pars1)))
    tmp1 <- pars_id_lorg(pars2, fit_ng_eq, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_ng_eq, type = "all")
    expect_true(all(paste0(tmp2$lhs, tmp2$op, tmp2$rhs) %in%
                    gsub(" ", "", pars2)))
    tmp1 <- pars_id_lorg(pars3, fit_ng_eq, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_ng_eq, type = "all")
    expect_true(all(paste0(tmp2$lhs, tmp2$op, tmp2$rhs) %in%
                    gsub(" ", "", pars3)))

    tmp1 <- pars_id_lorg(pars1, fit_gp, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_gp, type = "all")
    tmp2$group <- lavInspect(fit_gp, "group.label")[tmp2$group]
    expect_true(all(unique(paste0(tmp2$lhs, tmp2$op, tmp2$rhs)) %in%
                    gsub(" ", "", pars1)))
    tmp1 <- pars_id_lorg(pars2, fit_gp, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_gp, type = "all")
    tmp2$group <- lavInspect(fit_gp, "group.label")[tmp2$group]
    expect_true(any(gsub(" ", "", pars2) %in%
                    unique(paste0(tmp2$lhs, tmp2$op, tmp2$rhs))))
    expect_true(any(gsub(" ", "", pars2) %in%
                  unique(paste0(tmp2$lhs, tmp2$op, tmp2$rhs, ".", tmp2$group))))
    tmp1 <- pars_id_lorg(pars3, fit_gp, where = "partable")
    tmp2 <- pars_id_to_lorg(tmp1, pt_gp, type = "all")
    tmp2$group <- lavInspect(fit_gp, "group.label")[tmp2$group]
    expect_true(any(gsub(" ", "", pars3) %in%
                    unique(paste0(tmp2$lhs, tmp2$op, tmp2$rhs))))
    expect_true(any(gsub(" ", "", pars3) %in%
                  unique(paste0(tmp2$lhs, tmp2$op, tmp2$rhs, ".", tmp2$group))))

  })



# pars_id_op

pars1 <- c("f1 =~ x2", "f2 =~ x5", "=~", "f2 ~ f1")
pars2 <- c("f1 =~ x2", "~~.gp2", "f2 =~ x5.gp2", "f2 ~ f1", "=~.gp1")
pars3 <- c("f1 =~ x2", "~~", "f2 =~ x5.gp2", "~1.gp2",
           "f2 =~ x5.gp3", "f2 ~ f1.gp1")

# coef
test_that("pars_id_op: where = 'coef'", {
    expect_true(all.equal(pars_id_op(pars1, fit_ng),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "=~"), "free"]))
    expect_true(all.equal(pars_id_op(pars2, fit_ng), integer(0)))
    expect_true(all.equal(pars_id_op(pars3, fit_ng),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "~~"), "free"]))
    expect_true(all.equal(pars_id_op(pars1, fit_ng_eq),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "=~"), "free"]))
    expect_true(all.equal(pars_id_op(pars2, fit_ng_eq), integer(0)))
    expect_true(all.equal(pars_id_op(pars3, fit_ng_eq),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "~~"), "free"]))
    expect_true(all.equal(pars_id_op(pars1, fit_gp),
                          pt_gp[(pt_gp$free > 0) &
                                (pt_gp$op == "=~"), "free"]))
    expect_true(all.equal(pars_id_op(pars2, fit_gp),
                  pt_gp[(pt_gp$free > 0) &
                        (((pt_gp$op == "=~") & (pt_gp$group == 2)) |
                          ((pt_gp$op == "~~") & (pt_gp$group == 1))), "free"]))
    expect_true(all.equal(pars_id_op(pars3, fit_gp),
                  pt_gp[(pt_gp$free > 0) &
                        (((pt_gp$op == "~1") & (pt_gp$group == 1)) |
                        (pt_gp$op == "~~")), "free"]))
  })

test_that("pars_id_op: where = 'partable'", {
    expect_true(all.equal(pars_id_op(pars1, fit_ng, where = "partable"),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "=~"), "id"]))
    expect_true(all.equal(pars_id_op(pars2, fit_ng, where = "partable"),
                          integer(0)))
    expect_true(all.equal(pars_id_op(pars3, fit_ng, where = "partable"),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "~~"), "id"]))
    expect_true(all.equal(pars_id_op(pars1, fit_ng_eq, where = "partable"),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "=~"), "id"]))
    expect_true(all.equal(pars_id_op(pars2, fit_ng_eq, where = "partable"),
                          integer(0)))
    expect_true(all.equal(pars_id_op(pars3, fit_ng_eq, where = "partable"),
                          pt_ng[(pt_ng$free > 0) &
                                (pt_ng$op == "~~"), "id"]))
    expect_true(all.equal(pars_id_op(pars1, fit_gp, where = "partable"),
                          pt_gp[(pt_gp$free > 0) &
                                (pt_gp$op == "=~"), "id"]))
    expect_true(all.equal(pars_id_op(pars2, fit_gp, where = "partable"),
                  pt_gp[(pt_gp$free > 0) &
                        (((pt_gp$op == "=~") & (pt_gp$group == 2)) |
                          ((pt_gp$op == "~~") & (pt_gp$group == 1))), "id"]))
    expect_true(all.equal(pars_id_op(pars3, fit_gp, where = "partable"),
                  pt_gp[(pt_gp$free > 0) &
                        (((pt_gp$op == "~1") & (pt_gp$group == 1)) |
                        (pt_gp$op == "~~")), "id"]))
  })

test_that("pars_id_op: where = 'partable', type = 'all'", {
    expect_true(all.equal(pars_id_op(pars1, fit_ng, where = "partable",
                                     free_only = FALSE),
                          pt_ng[(pt_ng$free > -1) &
                                (pt_ng$op == "=~"), "id"]))
    expect_true(all.equal(pars_id_op(pars2, fit_ng, where = "partable",
                                     free_only = FALSE),
                          integer(0)))
    expect_true(all.equal(pars_id_op(pars3, fit_ng, where = "partable",
                                     free_only = FALSE),
                          pt_ng[(pt_ng$free > -1) &
                                (pt_ng$op == "~~"), "id"]))
    expect_true(all.equal(pars_id_op(pars1, fit_ng_eq, where = "partable",
                                     free_only = FALSE),
                          pt_ng[(pt_ng$free > -1) &
                                (pt_ng$op == "=~"), "id"]))
    expect_true(all.equal(pars_id_op(pars2, fit_ng_eq, where = "partable",
                                     free_only = FALSE),
                          integer(0)))
    expect_true(all.equal(pars_id_op(pars3, fit_ng_eq, where = "partable",
                                     free_only = FALSE),
                          pt_ng[(pt_ng$free > -1) &
                                (pt_ng$op == "~~"), "id"]))
    expect_true(all.equal(pars_id_op(pars1, fit_gp, where = "partable",
                                     free_only = FALSE),
                          pt_gp[(pt_gp$free > -1) &
                                (pt_gp$op == "=~"), "id"]))
    expect_true(all.equal(pars_id_op(pars2, fit_gp, where = "partable",
                                     free_only = FALSE),
                  pt_gp[(pt_gp$free > -1) &
                        (((pt_gp$op == "=~") & (pt_gp$group == 2)) |
                          ((pt_gp$op == "~~") & (pt_gp$group == 1))), "id"]))
    expect_true(all.equal(pars_id_op(pars3, fit_gp, where = "partable",
                                     free_only = FALSE),
                  pt_gp[(pt_gp$free > -1) &
                        (((pt_gp$op == "~1") & (pt_gp$group == 1)) |
                        (pt_gp$op == "~~")), "id"]))
  })
