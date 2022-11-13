skip("WIP")
library(testthat)
library(lavaan)

#context("Test mahalanobis_rerun.R")

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
pars_id <- function(pars,
                   fit) {
    pfree <- lavaan::lavInspect(fit, "npar")
    ngp <- lavaan::lavInspect(fit, "ngroups")
    ptable <- lavaan::parameterTable(fit)
    parspt <- tryCatch(lavaan::lavaanify(pars, ngroups = ngp),
                       error = function(e) e)
    if (inherits(parspt, "simpleError")) {
        stop(paste0("Error in parameter syntax. This is the lavaan error message:",
                    "\n",
                    parspt$message))
      }
    parspt2 <- as.data.frame(lavaan::lavParseModelString(pars))
    mcol <- c("lhs", "op", "rhs", "group", "free")
    parspt3 <- merge(parspt[, mcol],
                     parspt2)[, mcol]
    parspt3 <- parspt3[parspt3$free > 0, ]
    parspt4 <- merge(parspt3[, -which(mcol == "free")], ptable)
    out <- parspt4$free
    out
  }

(id11 <- pars_id(pars1, fit_ng))
(id12 <- pars_id(pars2, fit_ng))
(id13 <- pars_id(pars3, fit_ng))
(id21 <- pars_id(pars1, fit_gp))
(id22 <- pars_id(pars2, fit_gp))
(id23 <- pars_id(pars3, fit_gp))
pt_gp <- parameterTable(fit_gp)
tmp <- pt_gp[pt_gp$free %in% id23, ]
tmp[order(tmp$lhs, tmp$op, tmp$rhs, tmp$group), ]
