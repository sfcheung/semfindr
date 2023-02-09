skip("WIP")

library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ iv1 + a2 * iv2
dv ~ b * m1
a1b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
fit_est_change <- est_change(rerun_out)
fit_est_change_approx <- est_change_approx(fit)
fit_est_change_raw <- est_change_raw(rerun_out)
fit_est_change_raw_approx <- est_change_raw_approx(fit)
fit_est_change_raw_std <- est_change_raw(rerun_out,
                                         standardized = TRUE)
set.seed(89235)
n <- nrow(dat0)
random_case_ids <- paste0("case_", sample.int(n))
fit_est_change_approx_id <- est_change_approx(fit,
                                              case_id = random_case_ids)

gcd_method <- function(x,
                       gcd_names = c("gcd", "gcd_approx")) {
    gcd_name <- intersect(gcd_names, colnames(x))
    if (length(gcd_name) != 1) {
        stop("The method used cannot be determined")
      }
    gcd_name
  }

est_to_long <- function(x) {
    gcd_name <- gcd_method(x)
    tmp <- which(colnames(x) == gcd_name)
    x0 <- x[, -tmp]
    out <- data.frame(change = as.vector(x0))
    n <- nrow(x0)
    k <- ncol(x0)
    out$param <- rep(colnames(x0), each = n)
    out$case <- rep(rownames(x0), times = k)
    out[, gcd_name] <- rep(x[, gcd_name], times = k)
    attr(out, "gcd_name") <- gcd_name
    out
  }

tmp1 <- est_to_long(fit_est_change)
tmp2 <- est_to_long(fit_est_change_approx)
tmp3 <- est_to_long(fit_est_change_approx_id)

test_that("Test est_to_long", {
    expect_true("gcd" %in% colnames(tmp1))
    expect_true("gcd_approx" %in% colnames(tmp2))
    expect_equal(nrow(tmp1), nrow(fit_est_change) *
                            (ncol(fit_est_change) - 1))
    expect_equal(nrow(tmp2), nrow(fit_est_change_approx) *
                            (ncol(fit_est_change_approx) - 1))
  })

plot_est_change <- function(x,
                            params,
                            cutoff_gcd = NULL,
                            largest_gcd = 1) {
    gcd_name <- gcd_method(x)
    approx <- ifelse(gcd_name == "gcd_approx", TRUE, FALSE)
    x0 <- est_to_long(x)
    if (!missing(params)) {
        x0 <- x0[x0$param %in% params, ]
      }
    p <- ggplot2::ggplot(data = x0,
                ggplot2::aes(x = .data[[gcd_name]],
                             y = .data[["change"]])) +
          ggplot2::geom_point() +
          ggplot2::geom_hline(yintercept = 0)
    if (is.numeric(cutoff_gcd)) {
        p <- p + ggplot2::geom_vline(xintercept = cutoff_gcd,
                                     linetype = "dashed")
        c_gcd_cut <- cutoff_gcd
      } else {
        c_gcd_cut <- Inf
      }
    if (is.numeric(largest_gcd) && largest_gcd >= 1) {
        gcd_tmp <- x0[x0$param == x0$param[1], c("case",
                                                 gcd_name)]
        m_gcd <- round(largest_gcd)
        o_gcd <- order(gcd_tmp[[gcd_name]],
                               decreasing = TRUE)
        m_gcd_cut <- gcd_tmp[[gcd_name]][o_gcd[m_gcd]]
      } else {
        m_gcd_cut <- Inf
      }
    label_gcd <- (x0[[gcd_name]] >= c_gcd_cut) |
                 (x0[[gcd_name]] >= m_gcd_cut)
    p <- p + ggrepel::geom_label_repel(
                data = x0[label_gcd, ],
                ggplot2::aes(x = .data[[gcd_name]],
                             y = .data[["change"]],
                             label = .data[["case"]]),
                position = ggplot2::position_dodge(.5))
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["param"]]),
                        ncol = 1,
                        scales = "free_x",
                        strip.position = "left")
    p <- p + ggplot2::xlab(switch(gcd_name,
              gcd = "Generalized Cook's Distance",
              gcd_approx = "Generalized Cook's Distance (Approximated)"))
    p
  }

params <- c("m1~iv1", "a2", "b")
plot_est_change(fit_est_change, params = params)
plot_est_change(fit_est_change, params = params, largest_gcd = 3)
plot_est_change(fit_est_change, params = params, cutoff_gcd = .2)
params <- c("m1~iv1", "a2", "b")
plot_est_change(fit_est_change_raw, params = params)
params <- c("m1~iv1", "m1~iv2", "dv~m1")
plot_est_change(fit_est_change_approx, params = params)
plot_est_change(fit_est_change_approx, params = params, cutoff_gcd = .1)
plot_est_change(fit_est_change_approx, params = params, cutoff_gcd = .2)

# CFA model with selected loadings

mod <-
'
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f1 ~~ f2
'

dat <- cfa_dat

dat0 <- dat[1:50, ]
fit <- lavaan::cfa(mod, dat0)

fit_est_change_approx <- est_change_approx(fit, parameters = "=~")
params <- c("f1=~x2", "f2=~x5")
