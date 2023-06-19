skip_if_not_installed("modi")
library(testthat)
library(lavaan)

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

md_fit0 <- mahalanobis_rerun(fit0)
md_ordered <- order(md_fit0, decreasing = TRUE, na.last = NA)

md_top <- 4
md_selected <- md_ordered[seq_len(md_top)]
md_selected <- md_selected[!is.na(md_selected)]
rerun_md_top <- suppressWarnings(lavaan_rerun(fit0, to_rerun = md_selected, parallel = FALSE))
rerun_out <- suppressWarnings(lavaan_rerun(fit0, md_top = 4, parallel = FALSE))

md_fit <- mahalanobis_rerun(fit0)
md_fit_selected <- md_fit[rerun_out$selected, , drop = FALSE]

md_rerun <- mahalanobis_rerun(rerun_out)

#md_stats <- mahalanobis(dat0, colMeans(dat0), cov(dat0))

test_that("Compare Mahalanobis distances", {
    expect_equal(ignore_attr = TRUE,
        md_fit_selected,
        md_rerun
      )
  })
