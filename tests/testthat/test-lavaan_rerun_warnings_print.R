skip("To be tested in an interactive session")
library(testthat)
library(lavaan)
library(semfindr)

dat <- cfa_dat_mg
set.seed(866421)
dat <- dat[sample.int(nrow(dat), replace = TRUE), ]

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"

fit <- cfa(mod, dat,
                group = "gp",
                group.equal = c("loadings", "intercepts"))

rerun_config <- lavaan_rerun(fit)
rerun_config

rerun_config <- lavaan_rerun(fit,
                             to_rerun = c(64:67))
