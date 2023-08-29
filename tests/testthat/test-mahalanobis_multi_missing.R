skip_if_not_installed("modi")
library(testthat)
library(lavaan)

# Can handle missing data

mod <-
'
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:40, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)
dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[4, 1:4] <- NA
head(dat0)
suppressWarnings(fit0 <- lavaan::sem(mod, dat0, missing = "fiml.x",
                                     group = "gp"))

fit0_data <- lav_data_used(fit0)
head(fit0_data)
fit0_data_g1 <- fit0_data[fit0_data$gp == "gp2", -5]
fit0_data_g2 <- fit0_data[fit0_data$gp == "gp1", -5]
head(fit0_data_g1)
head(fit0_data_g2)

suppressWarnings(em_out_fit <- lavaan::lavCor(fit0_data_g1,
                                              missing = "fiml",
                                              output = "fit"))
em_out <- list(param = list())
tmp <- lavaan::lavInspect(em_out_fit, "implied")
em_out$param$beta <- tmp$mean
em_out$param$sigma <- tmp$cov
em_out_g1 <- em_out
md_check_g1 <- modi::MDmiss(fit0_data_g1,
                          em_out_g1$param$beta,
                          em_out_g1$param$sigma)
suppressWarnings(em_out_fit <- lavaan::lavCor(fit0_data_g2,
                                              missing = "fiml",
                                              output = "fit"))
em_out <- list(param = list())
tmp <- lavaan::lavInspect(em_out_fit, "implied")
em_out$param$beta <- tmp$mean
em_out$param$sigma <- tmp$cov
em_out_g2 <- em_out
md_check_g2 <- modi::MDmiss(fit0_data_g2,
                          em_out_g2$param$beta,
                          em_out_g2$param$sigma)
md_check <- c(md_check_g1, md_check_g2)

md_rerun <- mahalanobis_rerun(fit0)

test_that("Compare Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        sort(as.vector(md_rerun)),
        sort(md_check)
      )
  })
