skip_if_not_installed("modi")
library(testthat)
library(lavaan)


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
