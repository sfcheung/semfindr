skip("WIP")

library(testthat)
library(lavaan)
library(semfindr)

dat <- cfa_dat_heywood

mod <- 
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

fit <- lavaan::cfa(mod, dat)
fit_rerun <- lavaan_rerun(fit)
fit_post_check <- sapply(fit_rerun$rerun, function(x) {
                    chk <- tryCatch(lavaan::lavTech(x, what = "post.check"),
                                    warning = function(w) w)
                    })
i_valid <- sapply(fit_post_check, isTRUE)

fit_rerun2 <- lapply(seq_len(nrow(dat)), function(x) lavaan::cfa(mod, dat[-x, ]))
sapply(fit_rerun2, lavInspect, what = "converged")
sapply(fit_rerun2, lavInspect, what = "post.check")
mahalanobis_rerun(fit)
dat2 <- dat[-1, ]
fit_rerun22 <- lapply(seq_len(nrow(dat)), function(x) lavaan::cfa(mod, dat2[-x, ]))
sapply(fit_rerun2, lavInspect, what = "converged")
sapply(fit_rerun2, lavInspect, what = "post.check")
