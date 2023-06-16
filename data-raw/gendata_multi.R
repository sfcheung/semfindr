library(lavaan)

# A CFA model

mod_p1 <-
'
f1 =~  .7*x1 + .8*x2 + .6*x3
f2 =~  .4*x4 + .7*x5 + .4*x6
f1 ~~ .2*f2
'
mod_p2 <-
'
f1 =~  .7*x1 + .8*x2 + .6*x3
f2 =~  .4*x4 + .7*x5 + .4*x6
f1 ~~ .6*f2
'
mod <-
'
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
'

# generate data
set.seed(81166)
dat1 <- lavaan::simulateData(mod_p1, sample.nobs = 50L)
dat1$gp <- "GroupA"
set.seed(47136)
dat2 <- lavaan::simulateData(mod_p2, sample.nobs = 50L)
dat2$gp <- "GroupB"

dat <- rbind(dat1, dat2)
dat[40, 1:6] <- colMeans(dat[, 1:6]) + c(.2, .1, .1, .2, .1, .1)
dat[77, 1:6] <- dat[77, 1:6] + 1.6
head(dat)
dat0 <- dat[-100, ]
fit1b <- lavaan::cfa(mod, dat0, group = "gp")
fit2b <- lavaan::cfa(mod, dat0, group = "gp",
                    group.equal = c("loadings"))
fit3b <- lavaan::cfa(mod, dat0, group = "gp",
                    group.equal = c("loadings", "intercepts"))
print(mahalanobis_rerun(fit1b), first = 5)
lavTestLRT(fit1b, fit2b, fit3b)
lavTestLRT(fit1b, fit3b)
fitMeasures(fit1b, c("cfi", "rmsea"))
fitMeasures(fit3b, c("cfi", "rmsea"))

fit1b_rerun <- lavaan_rerun(fit1b)
fit1b_inf <- influence_stat(fit1b_rerun)
print(fit1b_inf, what = "parameters", first = 5)
print(fit1b_inf, what = "mahalanobis", first = 5)
gcd_plot(fit1b_inf,
         largest_gcd = 3)
md_plot(fit1b_inf,
        largest_md = 3)
gcd_gof_md_plot(fit1b_inf,
                fit_measure = "chisq",
                largest_gcd = 3,
                largest_fit_measure = 3,
                largest_md = 3,
                circle_size = 25)

fit3b_rerun <- lavaan_rerun(fit3b)
fit3b_inf <- influence_stat(fit3b_rerun)
print(fit3b_inf, what = "parameters", first = 5)
print(fit3b_inf, what = "mahalanobis", first = 5)
gcd_plot(fit3b_inf,
         largest_gcd = 3)
md_plot(fit3b_inf,
        largest_md = 3)
gcd_gof_md_plot(fit3b_inf,
                fit_measure = "chisq",
                largest_gcd = 3,
                largest_fit_measure = 3,
                largest_md = 3,
                circle_size = 15)

# dat[100, 1:6] <- dat2[50, 1:6] + c(1.7, 1.5, 1.4, -1.25, -1.4, -1.45) - 0
dat[100, 1:6] <- dat2[50, 1:6] + c(.7, -2, 1.75, .5, -2.4, 4.05) - 0
fit1 <- lavaan::cfa(mod, dat, group = "gp")
fit2 <- lavaan::cfa(mod, dat, group = "gp",
                    group.equal = c("loadings"))
fit3 <- lavaan::cfa(mod, dat, group = "gp",
                    group.equal = c("loadings", "intercepts"))
print(mahalanobis_rerun(fit1), first = 5)
lavTestLRT(fit1, fit2, fit3)
lavTestLRT(fit1, fit3)
fitMeasures(fit1, c("cfi", "rmsea"))
fitMeasures(fit3, c("cfi", "rmsea"))

fit1_rerun <- lavaan_rerun(fit1)
fit1_inf <- influence_stat(fit1_rerun)
print(fit1_inf, what = "parameters", first = 5)
print(fit1_inf, what = "mahalanobis", first = 5)
gcd_plot(fit1_inf,
         largest_gcd = 3)
md_plot(fit1_inf,
        largest_md = 3)
gcd_gof_md_plot(fit1_inf,
                fit_measure = "chisq",
                largest_gcd = 3,
                largest_fit_measure = 3,
                largest_md = 3,
                circle_size = 25)

fit3_rerun <- lavaan_rerun(fit3)
fit3_inf <- influence_stat(fit3_rerun)
print(fit3_inf, what = "parameters", first = 5)
print(fit3_inf, what = "mahalanobis", first = 5)
gcd_plot(fit3_inf,
         largest_gcd = 3)
md_plot(fit3_inf,
        largest_md = 3)
gcd_gof_md_plot(fit3_inf,
                fit_measure = "chisq",
                largest_gcd = 3,
                largest_fit_measure = 3,
                largest_md = 3,
                circle_size = 15)

fit1_compare <- parameterEstimates(fit1)[, c(1, 2, 3, 5, 6)]
fit1_compare$est_no_1 <- parameterEstimates(fit1b)$est
fit1_compare$diff <- fit1_compare$est - fit1_compare$est_no_1
fit1_compare[fit1_compare$op == "=~", ]
fit1_compare[(fit1_compare$op == "~~") & (fit1_compare$lhs != fit1_compare$rhs), ]
standardizedSolution(fit1)[c(15, 38), ]
standardizedSolution(fit1b)[c(15, 38), ]

fit3_compare <- parameterEstimates(fit3)[, c(1, 2, 3, 5, 7)]
fit3_compare$est_no_1 <- parameterEstimates(fit3b)$est
fit3_compare$diff <- fit3_compare$est - fit3_compare$est_no_1
fit3_compare[fit3_compare$op == "=~", ]
fit3_compare[(fit3_compare$op == "~~") & (fit3_compare$lhs != fit3_compare$rhs), ]
standardizedSolution(fit3)[c(15, 38), ]
standardizedSolution(fit3b)[c(15, 38), ]

cfa_dat_mg <- dat[, c("x1", "x2", "x3", "x4", "x5", "x6", "gp")]
head(cfa_dat_mg)
usethis::use_data(cfa_dat_mg, overwrite = TRUE)
