
#' @title Print a 'fit_measures_change' Class Object
#'
#' @description Print the content of a 'fit_measures_change'-class object.
#'
#' @details All the functions on case influence
#' on fit measures, [fit_measures_change()]
#' and [fit_measures_change_approx()], return
#' an `fit_measures_change`-class object. This method will print
#' the output, with the option to sort the cases.
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @param x An 'fit_measures_change'-class object.
#'
#' @param digits The number of digits after the decimal.
#' Default is 3.
#'
#' @param first Numeric. If not `NULL`, it prints
#' only the first *k* cases, *k* equal to `first`.
#' Default is 10.
#'
#' @param sort_by String. Default is `NULL` and
#' the output is not sorted. If set to a column
#' names of `x`, cases will sorted by this columns.
#' The sorting is done on the absolute values
#' if `absolute` is `TRUE`, and in decreasing
#' order if `decreasing` is `TRUE`. If `decrease`
#' is `FALSE`, the order is increasing. If `absolute`
#' is `FALSE`, the sorting is done on the raw values.
#'
#' @param decreasing Logical. Whether cases, if sorted,
#' is on decreasing order. Default is `TRUE`. See `sort_by`.
#'
#' @param absolute Logical. Whether cases, if sorted,
#' are sorted on absolute values. Default is `TRUE`.
#' See `sort_by`.
#'
#' @param ... Other arguments. They will be ignored.
#'
#' @seealso [fit_measures_change()], [fit_measures_change_approx()]
#'
#' @examples
#'
#' library(lavaan)
#'
#' # A path model
#'
#' dat <- pa_dat
#' mod <-
#' "
#' m1 ~ a1 * iv1 + a2 * iv2
#' dv ~ b * m1
#' a1b := a1 * b
#' a2b := a2 * b
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#'
#' # Case influence
#' out <- fit_measures_change_approx(fit)
#' out
#' print(out, sort_by = "chisq", first = 5)
#'
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = c(2, 3, 5, 7))#'
#' out <- fit_measures_change(fit_rerun)
#' out
#' print(out, sort_by = "chisq", first = 5)
#'
#' @export

print.fit_measures_change <- function(x,
                             digits = 3,
                             first = 10,
                             sort_by = NULL,
                             decreasing = TRUE,
                             absolute = TRUE,
                             ...) {
    if (is.null(first)) {
        first <- nrow(x)
      }
    first <- min(nrow(x), first)
    i <- seq_len(first)
    fm_method <- attr(x, "method")
    fm_call <- attr(x, "call")
    call_name <- as.character(fm_call[[1]])
    fm_names <- colnames(x)
    if (!is.null(sort_by)) {
        if (!isTRUE(sort_by %in% fm_names)) {
          stop("sort_by must have only one and only one",
               "of the column names of the object")
        }
      }
    if (!is.null(sort_by)) {
        xx <- x[, sort_by, drop = TRUE]
        if (absolute) {
            xx <- abs(xx)
          }
        j <- order(xx, decreasing = decreasing)
        out <- as.data.frame(x[j, , drop = FALSE])
      } else {
        out <- as.data.frame(x)
      }
    out <- out[i, , drop = FALSE]
    out <- round(out, digits = digits)
    tmp2 <- switch(fm_method,
                   leave_one_out = "",
                   approx = "Approximate ")

    cat("\n-- ",
        tmp2,
        "Case Influence on Fit Measures",
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

    if (!is.null(sort_by)) {
        cat("- Cases sorted by ", sort_by,
            " in ",
            ifelse(decreasing, "decreasing order",
                               "increasing order"),
            ifelse(absolute, " on absolute values",
                             ""),
            ".\n", sep = "")
      }

    invisible(x)
  }
