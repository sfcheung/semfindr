library(testthat)
library(lavaan)
 
# Can handle missing data

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[4, ] <- NA
head(dat0)
suppressWarnings(fit0 <- lavaan::sem(mod, dat0, missing = "fiml.x"))

fit0_data <- lavInspect(fit0, "data")
colnames(fit0_data) <- lavNames(fit0)
head(fit0_data)

em_out <- norm2::emNorm(fit0_data, estimate.worst = FALSE, criterion = 1e-6)
md_check <- modi::MDmiss(fit0_data,
                          em_out$param$beta,
                          em_out$param$sigma)

md_rerun <- mahalanobis_rerun(fit0)

test_that("Compare Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        as.vector(md_rerun),
        md_check
      )
  })
