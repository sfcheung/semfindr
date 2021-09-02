# Create several datasets for testing purpose.
# 

library(lavaan)
library(dplyr)
library(haven)

options(width = 132)

check_data <- function(model, data, pos = NULL, alpha = .05) {
    fit0 <- lavaan::sem(model = model, data = data, fixed.x = FALSE)
    n <- nrow(data)
    if (is.null(pos)) {
        pos <- n
      }
    fit1 <- lavaan::sem(model = model, data = data[-pos, ], fixed.x = FALSE)
    md <- stats::mahalanobis(data, colMeans(data), var(data))
    imax <- which(md == max(md))
    fit2 <- lavaan::sem(model = model, data = data[-imax, ], fixed.x = FALSE)

    est_std0 <- lavaan::standardizedSolution(fit0, ci = FALSE)
    est_std1 <- lavaan::standardizedSolution(fit1, ci = FALSE)
    est_std2 <- lavaan::standardizedSolution(fit2, ci = FALSE)
    est_std_all <- est_std0[, 1:3]
    est_std_all$est.std0 <- est_std0$est.std
    est_std_all$est.std1 <- est_std1$est.std
    est_std_change <- est_std1$est.std - est_std0$est.std
    est_std_all$est.change <- est_std_change
    est_std_all$p0 <- est_std0$pvalue
    est_std_all$p1 <- est_std1$pvalue
    est_sig_change <- !((est_std1$pvalue < alpha) == (est_std0$pvalue < alpha))
    est_std_all$sig.change1 <- est_sig_change
    est_std_all$est.std2 <- est_std2$est.std
    est_std_change <- est_std2$est.std - est_std0$est.std
    est_std_all$est.change2 <- est_std_change
    est_std_all$p2 <- est_std2$pvalue
    est_sig_change <- !((est_std2$pvalue < alpha) == (est_std0$pvalue < alpha))
    est_std_all$sig.change2 <- est_sig_change

    est_0 <- lavaan::parameterEstimates(fit0, ci = FALSE)
    est_1 <- lavaan::parameterEstimates(fit1, ci = FALSE)
    est_2 <- lavaan::parameterEstimates(fit2, ci = FALSE)
    est_all <- est_0[, 1:3]
    est_all$est.0 <- est_0$est
    est_all$est.1 <- est_1$est
    est_all$est.change1 <- est_all$est.1 - est_all$est.0
    est_all$est.2 <- est_2$est
    est_all$est.change2 <- est_all$est.2 - est_all$est.0
    est_all$std.change.1 <- (est_all$est.1 - est_all$est.0) / est_1$se
    est_all$std.change.2 <- (est_all$est.2 - est_all$est.0) / est_2$se
    est_all$p0 <- est_0$pvalue
    est_all$p1 <- est_1$pvalue
    est_all$p2 <- est_2$pvalue
    est_all$sig.change1 <- !((est_1$pvalue < alpha) == (est_0$pvalue < alpha))
    est_all$sig.change2 <- !((est_2$pvalue < alpha) == (est_0$pvalue < alpha))

    fitm0 <- fitMeasures(fit0, c("pvalue", "cfi", "tli", "rmsea"))
    fitm1 <- fitMeasures(fit1, c("pvalue", "cfi", "tli", "rmsea"))
    fitm2 <- fitMeasures(fit2, c("pvalue", "cfi", "tli", "rmsea"))
    fitm <- data.frame(cbind(fit0 = fitm0, fit1 = fitm1, fit2 = fitm2))
    rownames(fitm) <- names(fitm0)
    out <- list(fit0 = fit0,
                fit1 = fit1,
                fit2 = fit2,
                fitm = fitm,
                est_std0 = est_std0,
                est_std1 = est_std1,
                est_std2 = est_std2,
                est_std_all = est_std_all,
                est_all = est_all,
                md = md)
    out
  }

# Pa data with an influential case

# set.seed(8463104) Working seed.
set.seed(5354416324)
n <- 100
x1 <- rnorm(n)
x2 <- .3 * x1 + rnorm(n, 0, sqrt(1 - .3^2))
y1 <- .3 * x1 + .3 * x2 + rnorm(n, 0, sqrt(1 - .3^2 - .3^2 - 2 * .3 * .3 * .3))
y2 <- .3 * y1 + .05 * x1 + .10 * x2 + rnorm(n, 0, sqrt(1 - .3^2))
dat <- data.frame(x1, x2, y1, y2)
# dat[1, ] <- c(tmpx1 <- 2.81, 
#               tmpx2 <- 2.78, 
#               tmpy1 <- 1.45 * (.4 * tmpx1 + .3 * tmpx2),
#               1.45 * .6 * tmpy1)
dat[n, ] <- c(tmpx1 <- 1.51, 
              tmpx2 <- 1.52, 
              tmpy1 <- -1.5 * (.3 * tmpx1 + .3 * tmpx2),
              -1.4 * (.3 * tmpy1 + .3 * tmpx1 + .3* tmpx2))
dat <- round(dat, 2)
mod <- 
"
y1 ~ x1 + x2
y2 ~ y1
"
check <- check_data(mod, dat)
check$est_all
round(check$fitm, 3)
plot(check$md)

library(semfindr)
fit <- lavaan::sem(mod, dat, meanstructure = TRUE)
fit_rerun <- lavaan_rerun(fit, parallel = TRUE)
influence_out <- influence_stat(fit_rerun)
gcd_gof_md_plot(influence_out, fit_measure = "cfi", circle_size = 10)
md_plot(influence_out, largest_md = 3)
fit_implied_scores <- implied_scores(fit)
psych::pairs.panels(fit_implied_scores)

colnames(dat) <- c("iv1", "iv2", "m1", "dv")
dat_final <- cbind(case_id = paste0("id_", seq_len(n)), dat)
head(dat_final)
pa_dat2 <- dat_final
usethis::use_data(pa_dat2, overwrite = TRUE)

# CFA data with an influential case
mod <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"

tmpfct <- function(seed,
                   pertube = c(2.13, 1.91, 1.82, -1.82,  -1.58, -2.34)) {
  set.seed(seed)
  n <- 100
  f1 <- rnorm(n)
  f2 <- .1 * f1 + rnorm(n, 0, sqrt(1 - .1^2)) 
  fall <- cbind(f1, f2)
  lambda <- matrix(c(.5, -.05,
                    .6, .3,
                    .7, .1,
                    -.05, .5,
                    .2, .6,
                    .1, .7), 6, 2, byrow = TRUE)
  psi <- diag(2)
  psi[2, 1] <- psi[1, 2] <- .4
  epsilon <- diag(1 - diag(lambda %*% psi %*% t(lambda)))
  xall <- fall %*% t(lambda) + MASS::mvrnorm(n, rep(0, 6),
                                            epsilon)
  dat <- as.data.frame(round(xall, 2))
  colnames(dat) <- paste0("x", seq_len(ncol(dat)))
  if (!is.null(pertube)) {
      dat[n, ] <- pertube
    }
  dat <- round(dat, 2)
  # head(dat)
  # dat <- dat[-100, ]
  mod <- 
  "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
  "
  check <- check_data(mod, dat)
  goal <- (check$fitm["cfi", "fit0"] > .95) &
          (check$fitm["cfi", "fit2"] < .95) & 
          (check$fitm["rmsea", "fit0"] < .08) &
          (check$fitm["rmsea", "fit2"] > .08)
          
  out <- list(check = check,
              dat = dat,
              goal = goal,
              seed = seed)
  out
}

tmp <- tmpfct(1576156, pertube = c(-1.83, 1.81, -1.72, 1.22, 1.48, 1.34))
dat <- tmp$dat
check <- check_data(mod, dat)
check$est_all
round(check$fitm, 3)
plot(check$md)


library(semfindr)
fit <- lavaan::cfa(mod, dat, meanstructure = TRUE)
fit_rerun <- lavaan_rerun(fit, parallel = TRUE)
influence_out <- influence_stat(fit_rerun)
gcd_gof_md_plot(influence_out, fit_measure = "cfi", circle_size = 10)
md_plot(influence_out, largest_md = 3)

colnames(dat) <- paste0("x", 1:6)
dat_final <- cbind(case_id = paste0("id_", seq_len(n)), dat)
head(dat_final)
cfa_dat2 <- dat_final
usethis::use_data(cfa_dat2, overwrite = TRUE)


# SEM data with an influential case
mod <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f2 ~ f1
f3 ~ f2
"

tmpfct <- function(seed,
                   pertube = c(2.13,  1.91,  1.82,
                              -1.82, -1.58, -2.34,
                               2.32,  1.21,  1.23)) {
  set.seed(seed)
  n <- 100
  f1 <- rnorm(n)
  f2 <- .1 * f1 + rnorm(n, 0, sqrt(1 - .1^2))
  f3 <- .3 * f1 + .4 * f2 + rnorm(n, 0, sqrt(1 - .3^2 - .4^2 - 2*.3*.4))
  fall <- cbind(f1, f2, f3)
  lambda <- matrix(c(.5,  -.05, .1,
                     .6,   .1,  .1,
                     .7,   .1,  .1,
                    -.05,  .5,  .1,
                     .1,   .6,  .1,
                     .1,   .7,  .1,
                     .1,   .1,  .5,
                     .1,   .1,  .6,
                     .1,   .1,  .7), 9, 3, byrow = TRUE)
  psi <- diag(3)
  psi[2, 1] <- psi[1, 2] <- .1
  psi[3, 1] <- psi[1, 3] <- .3
  psi[3, 2] <- psi[2, 3] <- .4
  
  epsilon <- diag(1 - diag(lambda %*% psi %*% t(lambda)))
  xall <- fall %*% t(lambda) + MASS::mvrnorm(n, rep(0, 9),
                                            epsilon)
  dat <- as.data.frame(round(xall, 2))
  colnames(dat) <- paste0("x", seq_len(ncol(dat)))
  if (!is.null(pertube)) {
      dat[n, ] <- pertube
    }
  dat <- round(dat, 2)
  # head(dat)
  # dat <- dat[-100, ]
  mod <- 
  "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
  f3 =~ x7 + x8 + x9
  f2 ~ f1
  f3 ~ f2
  "
  check <- check_data(mod, dat)
  goal <- (check$fitm["cfi", "fit0"] > .95) &
          (check$fitm["cfi", "fit2"] < .95) & 
          (check$fitm["rmsea", "fit0"] < .08) &
          (check$fitm["rmsea", "fit2"] > .08)
          
  out <- list(check = check,
              dat = dat,
              goal = goal,
              seed = seed)
  out
}

tmp <- tmpfct(5625612, pertube = c( 1.83,  1.81,  1.72,
                                   -1.52, -1.48, -1.64,
                                    1.60,  1.89,  0.92))
dat <- tmp$dat
check <- check_data(mod, dat)
check$est_all
round(check$fitm, 3)
plot(check$md)


library(semfindr)
fit <- lavaan::cfa(mod, dat, meanstructure = TRUE)
fit_rerun <- lavaan_rerun(fit, parallel = TRUE)
influence_out <- influence_stat(fit_rerun)
gcd_gof_md_plot(influence_out, fit_measure = "cfi", circle_size = 10)
md_plot(influence_out, largest_md = 3)

colnames(dat) <- paste0("x", 1:9)
dat_final <- cbind(case_id = paste0("id_", seq_len(n)), dat)
head(dat_final)
sem_dat2 <- dat_final
usethis::use_data(sem_dat2, overwrite = TRUE)
