
#' @title Print a 'md_semfindr' Class Object
#'
#' @description Print the content of a 'md_semfindr'-class object.
#'
#' @details The print method for the
#' 'md_semfindr'-class object, returned by
#' [mahalanobis_rerun()] or [mahalanobis_predictors()].
#' This method will print
#' the output with the option to sort the cases.
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @param x An 'md_semfindr'-class object.
#'
#' @param digits The number of digits after the decimal.
#' Default is 3.
#'
#' @param first Numeric. If not `NULL`, it prints
#' only the first *k* cases, *k* equal to `first`.
#' Default is 10.
#'
#' @param sort Logical. If `TRUE`, the default, the cases
#' will be sorted based on Mahalanobis distance.
#' The order is determined by `decreasing`.
#'
#' @param decreasing Logical. Whether cases, if sorted,
#' is on decreasing order. Default is `TRUE`.
#'
#' @param ... Other arguments. They will be ignored.
#'
#' @seealso [mahalanobis_rerun()], [mahalanobis_predictors()]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- pa_dat
#' # The model
#' mod <-
#' "
#' m1 ~ a1 * iv1 + a2 * iv2
#' dv ~ b * m1
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#' # Fit the model n times. Each time with one case removed.
#' # For illustration, do this only for selected cases.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Compute the Mahalanobis distance for each case
#' out <- mahalanobis_rerun(fit_rerun)
#' out
#' print(out, first = 3)
#'
#' @export

print.md_semfindr <- function(x,
                              digits = 3,
                              first = 10,
                              sort = TRUE,
                              decreasing = TRUE,
                              ...) {
    if (is.null(first)) {
        first <- nrow(x)
      }
    first <- min(nrow(x), first)
    i <- seq_len(first)

    mh_call <- attr(x, "call")
    missing_data <- attr(x, "missing_data")
    call_name <- as.character(mh_call[[1]])
    md_na <- FALSE
    if (any(is.na(x[, "md"]))) {
        md_na <- TRUE
      }
    if (sort) {
        j <- order(x[, "md", drop = TRUE],
                   decreasing = decreasing)
        out <- as.data.frame(x[j, , drop = FALSE])
      } else {
        out <- as.data.frame(x)
      }
    out <- round(out, digits = digits)
    out <- out[i, , drop = FALSE]
    cat("\n-- ",
        "Mahalanobis Distance",
        " --\n", sep = "")
    cat("\n")
    print(out)

    cat("\nNote:\n")

    if (first != nrow(x)) {
        cat("- Only the first ",
            first,
            " case(s) is/are displayed.",
            " Set ", sQuote("first"),
            " to NULL to display all cases.",
            "\n", sep = "")
      } else {
        cat("- All stored cases are displayed.\n")
      }

    if (sort) {
        cat("- Cases sorted by Mahalanobis distance",
            " in ",
            ifelse(decreasing, "decreasing order",
                               "increasing order"),
            ".\n", sep = "")
      }

    if (missing_data) {
        cat("- Missing data is present. modi::MDmiss() was used.\n")
      }

    if (md_na) {
        cat("- Mahalanobis distance computation failed on one or more cases.\n")
      }

    exo_vars <- attr(x, "exo_vars")
    if (!is.null(exo_vars)) {
        cat("- Mahalanobis distance computed only on predictors.\n")
      }

    invisible(x)
  }
