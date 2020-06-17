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
fit <- lavaan::sem(mod, dat)
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

# A path analysis model with one influential case

mod_p <- 
'
m1 ~  0.5*iv1 + 0.6*iv2
dv ~  0.0*iv1 + 0.0*iv2 + 0.5*m1
'
mod <- 
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

# generate data
set.seed(7565354)
dat <- lavaan::simulateData(mod_p, sample.nobs = 100L)
dat[which.max(dat$m1), "m1"] <- -1*max(dat$m1)
fit <- lavaan::sem(mod, dat)
summary(fit, fit.measures = TRUE)

fit_rerun <- lavaan_rerun(fit)
out <- influence_stat(fit_rerun, fit_measures = "all")
apply(out, 2, which.max)
gcd_gof_md_plot(out, "cfi", cutoff_fit_measure = .02, circle_size = 15)
gcd_gof_md_plot(out, "rmsea", cutoff_fit_measure = .01, circle_size = 15)
fit1 <- lavaan::sem(mod, dat[-25, ])
fit1_rerun <- lavaan_rerun(fit1)
out1 <- influence_stat(fit1_rerun, fit_measures = "all")
gcd_gof_md_plot(out1, "cfi", cutoff_fit_measure = .02, circle_size = 15)
gcd_gof_md_plot(out1, "rmsea", cutoff_fit_measure = .015, circle_size = 15)

pa_dat_inf <- dat
usethis::use_data(pa_dat_inf, overwrite = TRUE)

# A CFA model with one influential case

mod_p <- 
'
f1 =~  .8*x1 + .8*x2 + .8*x3
f2 =~  .8*x4 + .8*x5 + .8*x6
f1 ~~ .4*f2
'
mod <- 
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

# generate data
set.seed(663756)
dat <- lavaan::simulateData(mod_p, sample.nobs = 200L)
dat[10, "x1"] <- -1.25*min(dat$x1)
dat[10, "x2"] <- -1.25*min(dat$x2)
dat[10, "x3"] <- -1.25*min(dat$x3)
dat[10, "x4"] <- -1.25*max(dat$x4)
dat[10, "x5"] <- -1.25*max(dat$x5)
dat[10, "x6"] <- -1.25*max(dat$x6)
fit <- lavaan::sem(mod, dat)
summary(fit, fit.measures = TRUE)

fit_rerun <- lavaan_rerun(fit)
out <- influence_stat(fit_rerun, fit_measures = "all")
apply(out, 2, which.max)
gcd_gof_md_plot(out, "cfi", cutoff_fit_measure = .025, circle_size = 15)
gcd_gof_md_plot(out, "rmsea", cutoff_fit_measure = .03, circle_size = 15)
fit1 <- lavaan::sem(mod, dat[-10, ])
fit1_rerun <- lavaan_rerun(fit1)
out1 <- influence_stat(fit1_rerun, fit_measures = "all")
gcd_gof_md_plot(out1, "cfi", cutoff_fit_measure = .025, circle_size = 15)
gcd_gof_md_plot(out1, "rmsea", cutoff_fit_measure = .03, circle_size = 15)

cfa_dat_inf <- dat
usethis::use_data(cfa_dat_inf, overwrite = TRUE)

# An SEM model with one influential case

mod_p <- 
'
f1 =~  .7*x1 + .6*x2 + .8*x3 + .3*x5
f2 =~  .0*x1 + .6*x4 + .8*x5 + .7*x6
f3 =~  .0*x4 + .2*x7 + .6*x8 + .8*x9
f2 ~   .3*f1
f3 ~   .3*f2 + .0*f1
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
set.seed(76754653)
dat <- lavaan::simulateData(mod_p, sample.nobs = 200L)
dat[10, "x1"] <- -1.25*min(dat$x1)
dat[10, "x2"] <- -1.25*min(dat$x2)
dat[10, "x3"] <- -1.25*min(dat$x3)
dat[10, "x4"] <- -1.25*max(dat$x4)
dat[10, "x5"] <- -1.25*max(dat$x5)
dat[10, "x6"] <- -1.25*max(dat$x6)
dat[10, "x7"] <- -1.25*min(dat$x7)
dat[10, "x8"] <- -1.25*min(dat$x8)
dat[10, "x9"] <- -1.25*min(dat$x9)
fit <- lavaan::sem(mod, dat)
summary(fit, fit.measures = TRUE)

fit_rerun <- lavaan_rerun(fit)
out <- influence_stat(fit_rerun, fit_measures = "all")
apply(out, 2, which.max)
gcd_gof_md_plot(out, "cfi", cutoff_fit_measure = .025, circle_size = 15)
gcd_gof_md_plot(out, "rmsea", cutoff_fit_measure = .03, circle_size = 15)
fit1 <- lavaan::sem(mod, dat[-10, ])
fit1_rerun <- lavaan_rerun(fit1)
out1 <- influence_stat(fit1_rerun, fit_measures = "all")
gcd_gof_md_plot(out1, "cfi", cutoff_fit_measure = .025, circle_size = 15)
gcd_gof_md_plot(out1, "rmsea", cutoff_fit_measure = .03, circle_size = 15)


sem_dat_inf <- dat
usethis::use_data(sem_dat_inf, overwrite = TRUE)

