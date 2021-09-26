# Create a data set that cannot converge.
#

library(lavaan)

# A CFA model

mod_p <- 
'
f1 =~  .7*x1 + .6*x2 + .8*x3 + .3*x5
f2 =~  .2*x1 + .6*x4 + .8*x5 + .7*x6
f1 ~~ .2*f2
'
mod <- 
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

# generate data
# n = 60L
# Working: set.seed(256551)
# Working: set.seed(465531)
# Working: set.seed(754364)
# Working: set.seed(817775)
# Working: set.seed(1817775)
set.seed(1817775)
dat <- lavaan::simulateData(mod_p, sample.nobs = 60L)
round(cor(dat), 2)
psych::pairs.panels(dat)
fit <- lavaan::cfa(mod, dat)
fit_rr <- lapply(seq_len(nrow(dat)), function(x) {
    lavaan::cfa(mod, dat[-x, ])
  })
sapply(fit_rr, lavInspect, what = "converged")
sapply(fit_rr, lavInspect, what = "post.check")
standardizedSolution(fit)[, c(1:4)]
dat2 <- dat
dat2[1, ] <- dat[1, ] + c(-2.95, 3.35, -3.5, 0, 0, 0)
round(cor(dat2), 2)
psych::pairs.panels(dat2)
fit2 <- lavaan::cfa(mod, dat2)
standardizedSolution(fit2)[, c(1:4)]
fit2_rr <- lapply(seq_len(nrow(dat2)), function(x) {
    lavaan::cfa(mod, dat2[-x, ])
  })
sapply(fit2_rr, lavInspect, what = "converged")
sapply(fit2_rr, lavInspect, what = "post.check")
coef(fit2)
tmp <- t(sapply(fit2_rr, coef))
max(tmp[, "x1~~x1"])

semfindr::mahalanobis_rerun(fit2)

dat3 <- dat2[-1, ]
fit3 <- lavaan::cfa(mod, dat3)
fit3_rr <- lapply(seq_len(nrow(dat3)), function(x) {
    lavaan::cfa(mod, dat3[-x, ])
  })
sapply(fit3_rr, lavInspect, what = "converged")
sapply(fit3_rr, lavInspect, what = "post.check")

cfa_dat_heywood <- dat2
usethis::use_data(cfa_dat_heywood, overwrite = TRUE)
