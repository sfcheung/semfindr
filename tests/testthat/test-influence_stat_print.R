library(testthat)
library(lavaan)
library(semfindr)

# NOTE
# Do this after the print methods for
# other functions it calls are ready.

#context("Test influence_stat.R")

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)

md_rerun   <- mahalanobis_rerun(rerun_out)
fm_rerun   <- fit_measures_change(rerun_out)
es_rerun   <- est_change(rerun_out)

in_rerun   <- influence_stat(rerun_out)

#' @title Print an 'influence_stat' Class Object
#'
#' @description Print the content of an 'influence_stat'-class object.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @param x An 'influence_stat'-class object.
#' @param ...  Optional arguments.
#'
#'
#' @seealso [influence_stat()]
#'
#' @examples
#'
#' x <- 123
#' print(x)
#'
#' @export
#' @describeIn topic Description of this function
#' @order 1

print.influence_stat <- function(x,
                                 digits = 3,
                                 parameters = TRUE,
                                 fit_measures = TRUE,
                                 mahalanobis = TRUE,
                                 first = NULL,
                                 sort = TRUE) {
    if (is.null(first)) {
        first <- nrow(x)
      }
    i <- seq_len(first)

    parameters_names <- attr(x, "parameters_names")
    if (parameters && !is.null(parameters_names)) {
        gcd_name <- switch(attr(x, "method"),
                           rerun = "gcd",
                           approx = "gcd_approx")
        gcd_name2 <- switch(gcd_name,
                            gcd = "generalized Cook's distance",
                            approx = "approximated generalized Cook's distance")
        if (sort) {
            out_est <- x[order(x[, gcd_name, drop = FALSE],
                               decreasing = TRUE),
                         parameters_names]
            out_est <- out_est[i, , drop = FALSE]
          } else {
            out_est <- x[i, parameters_names, drop = FALSE]
          }
        out_est <- round(as.data.frame(out_est), digits)
        colnames(out_est) <- parameters_names
        cat("\n",
             "-- Case Influence on Parameter Estimates (with gCD) --")
        if (sort) {
            cat("\n",
                "Sorted by",
                gcd_name2)
          }
        cat("\n\n")
        print(out_est)
      }

    fit_measures_names <- attr(x, "fit_measures_names")
    if (fit_measures && !is.null(fit_measures_names)) {
        out_fm <- x[i, fit_measures_names]
        out_fm <- round(as.data.frame(out_fm), digits)
        colnames(out_fm) <- fit_measures_names
        cat("\n",
             "-- Case Influence on Fit Measures --",
             "\n\n")
        print(out_fm)
      }

    mahalanobis_names <- attr(x, "mahalanobis_names")
    if (mahalanobis && !is.null(mahalanobis_names)) {
        out_mh <- x[i, mahalanobis_names]
        out_mh <- round(as.data.frame(out_mh), digits)
        colnames(out_mh) <- mahalanobis_names
        cat("\n",
             "-- Mahalanobis Distances --",
             "\n\n")
        print(out_mh)
      }
    invisible(x)
  }

print(in_rerun, first = 10)
