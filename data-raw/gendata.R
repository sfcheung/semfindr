# Create several datasets for testing purpose.
# 

library(lavaan)

# A path analysis model

mod_p <- 
'
m1 ~  0.5*iv1 + 0.6*iv2
dv ~  0.4*iv1 + 0.1*iv2 + 0.5*m1
'
mod <- 
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

# generate data
set.seed(7325876)
dat <- lavaan::simulateData(mod_p, sample.nobs = 100L)
fit <- lavaan::sem(mod, dat)

pa_dat <- dat
usethis::use_data(pa_dat, overwrite = TRUE)

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
set.seed(56453451)
dat <- lavaan::simulateData(mod_p, sample.nobs = 100L)
fit <- lavaan::cfa(mod, dat)
summary(fit, fit.measures = TRUE)

cfa_dat <- dat
usethis::use_data(cfa_dat, overwrite = TRUE)



# An SEM model

mod_p <- 
'
f1 =~  .7*x1 + .6*x2 + .8*x3 + .3*x5
f2 =~  .2*x1 + .6*x4 + .8*x5 + .7*x6
f3 =~  .5*x4 + .2*x7 + .6*x8 + .8*x9
f2 ~   .3*f1
f3 ~   .3*f2 + .8*f1
'
mod <- 
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f2 ~  f1
f3 ~  f2
'

# generate data
set.seed(7657634)
dat <- lavaan::simulateData(mod_p, sample.nobs = 200L)
fit <- lavaan::sem(mod, dat)
summary(fit, fit.measures = TRUE)

sem_dat <- dat
usethis::use_data(sem_dat, overwrite = TRUE)

