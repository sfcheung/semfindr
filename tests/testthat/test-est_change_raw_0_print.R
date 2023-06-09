skip("To be tested in an interactive session")
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

dat0 <- dat[1:25, ]
fit0 <- lavaan::sem(mod, dat0)
fit0_15 <- lavaan::sem(mod, dat0[-15, ])

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]

est0 <- lavaan::parameterEstimates(fit0, standardized = TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, standardized = TRUE)
est_change_rerun_all <- est_change_raw(rerun_out)
est_change_rerun_all_std <- est_change_raw(rerun_out, standardized = TRUE)
est_change_rerun_all_paths <- est_change_raw(rerun_out,
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))
est_change_rerun_all_paths_std <- est_change_raw(rerun_out,
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"),
                                standardized = TRUE)


#' @title Print an 'est_change' Class Object
#'
#' @description Print the content of an 'est_change'-class object.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @param x An 'est_change'-class object.
#' @param ...  Optional arguments.
#'
#'
#' @seealso [est_change_raw()]
#'
#' @examples
#'
#' x <- 123
#' print(x)
#'
#' @export
#' @describeIn topic Description of this function
#' @order 1

print.est_change <- function(x,
                             digits = 3,
                             first = NULL,
                             sort = TRUE) {
    if (is.null(first)) {
        first <- nrow(x)
      }
    i <- seq_len(first)
    est_type <- attr(x, "type")
    est_call <- attr(x, "call")
    est_std <- attr(x, "standardized")
    call_name <- as.character(est_call[[1]])
    est_method <- attr(x, "method")
    gcd_name <- switch(call_name,
                       est_change_raw = NULL,
                       est_change_raw_approx = NULL,
                       est_change = "gcd",
                       est_change_approx = "gcd_approx")
    if (!is.null(gcd_name)) {
        gcd_name2 <- switch(gcd_name,
                            gcd = "generalized Cook's distance",
                            approx = "approximated generalized Cook's distance")
      } else {
        gcd_name2 <- NULL
      }
    pnames <- switch(est_type,
                     raw = colnames(x))
    id <- rownames(x)

    if (identical(est_type, "raw")) {
        if (sort) {
            fct <- function(pname, xx, digits) {
                out_1 <- xx[order(abs(xx[, pname]), decreasing = TRUE),
                            pname, drop = FALSE]
                out_2 <- data.frame(id = rownames(out_1),
                                    p = round(out_1[, pname],
                                              digits = digits))
                out_2 <- out_2[i, ]
                colnames(out_2) <- c("id", pname)
                out_2
              }
            out <- lapply(pnames, fct, xx = x, digits = digits)
            out <- do.call(cbind, out)
            rownames(out) <- NULL
          } else {
            out <- as.data.frame(round(x, digits = digits))
            out <- out[i, , drop = FALSE]
          }
      }
    tmp <- ifelse(est_std, "Standardized Parameter Estimates",
                           "Parameter Estimates")
    cat("\n-- Case Influence on",
        tmp,
        "--")
    cat("\n\n")
    print(out)

    cat("\nNote:\n")

    if (identical(est_type, "raw"))
    cat("- Changes are raw changes on estimates.\n")

    if (first != nrow(x)) {
        cat("- Only the first", first, "cases are displayed.\n")
      }

    if (sort) {
        if (identical(est_type, "raw"))
        cat("- Cases sorted by the absolute raw changes for each variable.\n")
      }

    invisible(x)
  }

print(est_change_rerun_all)
print(est_change_rerun_all, first = 10)
print(est_change_rerun_all, first = 10, sort = FALSE)
print(est_change_rerun_all_std, first = 10)
print(est_change_rerun_all_paths, first = 10)
print(est_change_rerun_all_paths_std, first = 10)

