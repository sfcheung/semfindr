# Create several datasets for testing purpose.
# 
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
