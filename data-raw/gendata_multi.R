library(lavaan)

# A CFA model

mod_p1 <-
'
f1 =~  .7*x1 + .8*x2 + .8*x3 + .4*x6
f2 =~  .4*x4 + .8*x5 + .4*x6 + .2*x3
f1 ~~ .2*f2
'
mod_p2 <-
'
f1 =~  .7*x1 + .5*x2 + .8*x3 + .2*x5
f2 =~  .6*x4 + .7*x5 + .7*x6 + .4*x1
f1 ~~ .6*f2
'
mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

# generate data
set.seed(15443)
dat1 <- lavaan::simulateData(mod_p1, sample.nobs = 50L)
dat1$gp <- "GroupA"
set.seed(84342)
dat2 <- lavaan::simulateData(mod_p2, sample.nobs = 50L)
dat2$gp <- "GroupB"
dat <- rbind(dat1, dat2)
head(dat)
fit0b <- lavaan::cfa(mod, dat[-1, ], group = "gp")
fit1b <- lavaan::cfa(mod, dat[-1, ], group = "gp",
                    group.equal = c("loadings"))
fit2b <- lavaan::cfa(mod, dat[-1, ], group = "gp",
                    group.equal = c("loadings", "intercepts"))
dat[1, 1:6] <- dat1[1, 1:6] + c(3.5, -3.7, 0, 0, -4.5, 5.5) - .5
fit0 <- lavaan::cfa(mod, dat, group = "gp")
fit1 <- lavaan::cfa(mod, dat, group = "gp",
                    group.equal = c("loadings"))
fit2 <- lavaan::cfa(mod, dat, group = "gp",
                    group.equal = c("loadings", "intercepts"))
lavTestLRT(fit0b, fit1b, fit2b)
lavTestLRT(fit0, fit1, fit2)
lavTestLRT(fit0b, fit2b)
lavTestLRT(fit0, fit2)

cfa_dat_mg <- dat[, c("x1", "x2", "x3", "x4", "x5", "x6", "gp")]
head(cfa_dat_mg)
usethis::use_data(cfa_dat_mg, overwrite = TRUE)
