#' @title Print an 'influence_stat' Class Object
#'
#' @description Print the content of an 'influence_stat'-class object.
#'
#' @details This method will print
#' the output of [influence_stat()] in a user-friendly
#' way. Users can select the set(s) of output,
#' case influence on parameter estimates,
#' case influence on fit measures, and
#' Mahalanobis distance, to be printed.
#' The corresponding print methods of
#' `est_change`-class objects,
#' `fit_measures_change`-class objects,
#' and `md_semfindr`-class objects will be called.
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @param x An 'influence_stat'-class object.
#'
#' @param digits The number of digits after the decimal.
#' Default is 3.
#'
#' @param what A character vector of the
#' results #' to be printed, can be
#' one or more of the following:
#' `"parameters"`, `"fit_measures"`,
#' and `"mahalanobis"`. Default
#' is `c("parameters", "fit_measures", "mahalanobis")`.
#'
#' @param first Numeric. If not `NULL`, it prints
#' only the first *k* cases, *k* equal to `first`.
#' Default is 10.
#'
#' @param sort_parameters_by String.
#' If it is `"est"`, the cases are sorted individually
#' on each columns. If it is `"gcd"`,
#' the default,
#' then cases are sorted by generalized Cook's distance
#' or approximate generalized Cook's distance, depending on
#' which column is available.
#' If `NULL`, cases are not sorted.
#'
#' @param sort_fit_measures_by String. Default is `NULL` and
#' the output of case influence on fit measures is not
#' sorted. If set to a column
#' names of case influence on fit measures , cases will
#' sorted by these columns.
#' The sorting is done on the absolute values
#' if `sort_fit_measures_on_absolute` is `TRUE`, and in decreasing
#' order if `decreasing` is `TRUE`. If `decrease`
#' is `FALSE`, the order is increasing. If `sort_fit_measures_on_absolute`
#' is `FALSE`, the sorting is done on the raw values.
#'
#' @param sort_mahalanobis Logical. If `TRUE`, the default, the cases
#' in the output of Mahalanobis distance
#' will be sorted based on Mahalanobis distance.
#' The order is determined by `sort_mahalanobis_decreasing`.
#'
#' @param sort_fit_measures_decreasing Logical. Whether cases, if sorted
#' on fit measures,
#' are on decreasing order in the output of
#' case influence on fit measures. Default is `TRUE`.
#'
#' @param sort_fit_measures_on_absolute Logical. Whether
#' cases, if sorted on fit measures,
#' are sorted on absolute values of fit measures. Default is `TRUE`.
#' See `sort_fit_measures_by`.
#'
#' @param sort_mahalanobis_decreasing Logical. Whether cases, if sorted
#' on Mahalanobis distance,
#' is on decreasing order. Default is `TRUE`.
#'
#' @param ...  Optional arguments. Passed to
#' other print methods, such as [print.est_change()],
#' [print.fit_measures_change()], and [print.md_semfindr()].
#'
#' @seealso [influence_stat()], [print.est_change()],
#' [print.fit_measures_change()], [print.md_semfindr()]
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # The model
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
#' # --- Leave-One-Out Approach
#'
#' # Fit the model n times. Each time with one case removed.
#' # For illustration, do this only for selected cases.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Get all default influence stats
#' out <- influence_stat(fit_rerun)
#' out
#' print(out, first = 4)
#' print(out, what = c("parameters", "fit_measures"))
#'
#' # --- Approximate Approach
#'
#' out_approx <- influence_stat(fit)
#' out_approx
#' print(out, first = 8)
#' print(out, what = c("parameters", "fit_measures"),
#'       sort_parameters_by = "est")
#'
#' @export

print.influence_stat <- function(x,
                                 digits = 3,
                                 what = c("parameters",
                                          "fit_measures",
                                          "mahalanobis"),
                                 first = 10,
                                 sort_parameters_by = c("gcd", "est"),
                                 sort_fit_measures_by = NULL,
                                 sort_mahalanobis = TRUE,
                                 sort_fit_measures_decreasing = TRUE,
                                 sort_fit_measures_on_absolute = TRUE,
                                 sort_mahalanobis_decreasing = TRUE,
                                 ...) {
    what <- match.arg(what, several.ok = TRUE)
    if ("parameters" %in% what) {
        pnames <- attr(x, "parameters_names")
        if (is.null(pnames)) {
            warnings("Case influence on parameters not available.")
          }
        xx <- x[, pnames, drop = FALSE]
        attributes(xx) <- attr(x, "parameters_attrs")
        class(xx) <- c("est_change", class(xx))
        print(xx,
              digits = digits,
              first = first,
              sort_by = sort_parameters_by,
              ...)
      }

    if ("fit_measures" %in% what) {
        fnames <- attr(x, "fit_measures_names")
        if (is.null(fnames)) {
            warnings("Case influence on fit measures not available.")
          }
        xx <- x[, fnames, drop = FALSE]
        attributes(xx) <- attr(x, "fit_measures_attrs")
        class(xx) <- c("fit_measures_change", class(xx))
        print(xx,
              digits = digits,
              first = first,
              sort_by = sort_fit_measures_by,
              decreasing = sort_fit_measures_decreasing,
              absolute = sort_fit_measures_on_absolute,
              ...)
      }

    if ("mahalanobis" %in% what) {
        mnames <- attr(x, "mahalanobis_names")
        if (is.null(mnames)) {
            warnings("Mahalanobis distance not available.")
          }
        xx <- x[, mnames, drop = FALSE]
        attributes(xx) <- attr(x, "mahalanobis_attrs")
        class(xx) <- c("md_semfindr", class(xx))
        print(xx,
              digits = 3,
              first = first,
              sort = sort_mahalanobis,
              decreasing = sort_mahalanobis_decreasing,
              ...)
      }

    invisible(x)
  }
