#' @title Index Plot of an Arbitrary Statistic
#'
#' @description A generic index plot
#' function for plotting values of
#' a column # in a matrix.
#'
#' @details This index plot function
#' is for plotting any measure of
#' influence or extremeness in a matrix.
#' It can be used for measures not supported
#' with other functions.
#'
#' Like functions such as [gcd_plot()]
#' and [est_change_plot()], it supports
#' labelling cases based on the values
#' on the selected measure
#' (originaL values or absolute values).
#'
#' Users can also plot cases based on
#' the absolute values. This is useful
#' when cases are to be compared on
#' magnitude, ignoring the sign.
#'
#' @param object A matrix-like object,
#' such as the output from
#' [influence_stat()], [est_change()],
#' [est_change_raw()], and their
#' counterparts for the approximate
#' approach.
#'
#' @param column String. The column
#' name of the values to be plotted.
#'
#' @param plot_title The title of the
#' plot. Default is `"Index Plot"`.
#'
#' @param x_label The Label for the
#' vertical axis, for the value of
#' `column`. Default is `NULL`.
#' If `NULL`, then the label is
#' changed to
#' `"Statistic"` if `absolute` is
#' `FALSE`, and `"Absolute(Statistics)"`
#' if `absolute` is `TRUE`.
#'
#' @param cutoff_x_low Cases with values
#' smaller than this value will be labeled.
#' A cutoff line will be drawn at this
#' value.
#' Default is `NULL`. If `NULL`, no
#' cutoff line will be drawn for this
#' value.
#'
#' @param cutoff_x_high Cases with values
#' larger than this value will be labeled.
#' A cutoff line will be drawn at this
#' value.
#' Default is `NULL`. If `NULL`, no
#' cutoff line will be drawn for this
#' value.
#'
#' @param largest_x The number of cases
#' with the largest absolute value on
#' `column``
#' to be labelled. Default is 1. If not
#' an integer, it will be rounded to the
#' nearest integer.
#'
#' @param absolute Whether absolute values
#' will be plotted. Useful when cases
#' are to be compared on magnitude,
#' ignoring sign. Default is `FALSE`.
#'
#' @param point_aes A named list of
#' arguments to be passed to
#' [ggplot2::geom_point()] to modify how
#' to draw the points. Default is
#' `list()` and internal default
#' settings will be used.
#'
#' @param vline_aes A named list of
#' arguments to be passed to
#' [ggplot2::geom_segment()] to modify how
#' to draw the line for each case
#' in the index plot. Default is
#' `list()` and internal default
#' settings will be used.
#'
#' @param hline_aes A named list of
#' arguments to be passed to
#' [ggplot2::geom_hline()] to modify how
#' to draw the horizontal line for zero
#' case influence. Default is `list()`
#' and internal default settings will be
#' used.
#'
#' @param cutoff_line_aes A named list
#' of arguments to be passed to
#' [ggplot2::geom_hline()] to modify how
#' to draw the line for user cutoff
#' values. Default is `list()`
#' and internal default settings will be
#' used.
#'
#' @param case_label_aes A named list of
#' arguments to be passed to
#' [ggrepel::geom_label_repel()] to
#' modify how to draw the labels for
#' cases marked (based on arguments
#' such as `cutoff_x_low` or `largest_x`).
#' Default is `list()` and internal
#' default settings will be used.
#'
#' @return A [ggplot2] plot. Plotted by
#' default. If assigned to a variable
#' or called inside a function, it will
#' not be plotted. Use [plot()] to
#' plot it.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
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
#'
#' # Plot case influence on chi-square. Label the 3 cases with the influence.
#' index_plot(out, "chisq", largest_x = 3)
#'
#' # Plot absolute case influence on chi-square.
#' index_plot(out, "chisq", absolute = TRUE)
#'
#' @seealso [influence_stat()], [est_change()],
#' [est_change_raw()]
#'
#' @importFrom rlang .data
#' @export

index_plot <- function(object,
                       column = NULL,
                       plot_title = "Index Plot",
                       x_label = NULL,
                       cutoff_x_low = NULL,
                       cutoff_x_high = NULL,
                       largest_x = 1,
                       absolute = FALSE,
                       point_aes = list(),
                       vline_aes = list(),
                       hline_aes = list(),
                       cutoff_line_aes = list(),
                       case_label_aes = list()
                       ) {

  if (is.null(dim(object))) {
      if (!is.vector(object)) {
          stop("'object' invalid. Neither a matrix nor a vector.")
        }
      if (is.null(names(object))) {
          stop("If 'object' is a vector, it must be named.")
        }
    } else {
      if (length(column) != 1) {
          stop("'column' must has a length of 1.")
        }
      column <- as.character(column)
      if (!(column %in% colnames(object))) {
          stop(sQuote(column), " not found in 'object'.")
        }
      object <- object[, column, drop = TRUE]
    }

  object <- object[!is.na(object)]
  if (length(object) == 0) {
      stop("No cases have valid values.")
    }

  if (absolute) {
      object <- abs(object)
    }

  if (is.null(x_label)) {
      x_label <- ifelse(absolute,
                        "Absolute(Statistic)",
                        "Statistic")
    }

  point_aes <- utils::modifyList(list(),
                                 point_aes)

  vline_aes <- utils::modifyList(list(linewidth = 1,
                                      lineend = "butt"),
                                  vline_aes)
  # The following part should never be changed by users.
  vline_aes <- utils::modifyList(vline_aes,
                                  list(mapping = ggplot2::aes(
                                            xend = .data[["row_id"]],
                                            yend = 0)))
  hline_aes <- utils::modifyList(list(linetype = "solid",
                                      color = "grey"),
                                  hline_aes)
  # The following part should never be changed by users.
  hline_aes <- utils::modifyList(hline_aes,
                                  list(yintercept = 0))

  case_ids <- names(object)
  row_id   <- seq_len(length(object))
  dat <- data.frame(row_id = row_id,
                    case_id = case_ids,
                    x = object,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$row_id, .data$x))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + ggplot2::labs(title = plot_title)
  p <- p + do.call(ggplot2::geom_segment, vline_aes)
  p <- p + do.call(ggplot2::geom_hline, hline_aes)
  p <- p + ggplot2::xlab("Case ID (or Row Number)") +
         ggplot2::ylab(x_label)

  if (is.numeric(cutoff_x_low)) {
      cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"),
                                                cutoff_line_aes)
      # The following part should never be changed by users.
      cutoff_line_aes <- utils::modifyList(cutoff_line_aes,
                                    list(yintercept = cutoff_x_low))
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes)
      c_x_cut_low <- cutoff_x_low
    } else {
      c_x_cut_low <- -Inf
    }
  if (is.numeric(cutoff_x_high)) {
      cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"),
                                                cutoff_line_aes)
      # The following part should never be changed by users.
      cutoff_line_aes <- utils::modifyList(cutoff_line_aes,
                                    list(yintercept = cutoff_x_high))
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes)
      c_x_cut_high <- cutoff_x_high
    } else {
      c_x_cut_high <- Inf
    }


  if (is.numeric(largest_x) && largest_x >= 1) {
      m_x <- round(largest_x)
      o_x <- order(abs(dat$x), decreasing = TRUE)
      m_x_cut <- abs(dat$x)[o_x[m_x]]
    } else {
      m_x_cut <- Inf
    }
  label_x <- (dat$x >= c_x_cut_high) |
             (dat$x <= c_x_cut_low) |
             (abs(dat$x) >= m_x_cut)

  case_label_aes <- utils::modifyList(list(position = ggplot2::position_dodge(.5)),
                                  case_label_aes)
  # The following part should never be changed by users.
  case_label_aes <- utils::modifyList(case_label_aes,
                        list(data = dat[label_x, ],
                             mapping = ggplot2::aes(
                                x = .data[["row_id"]],
                                y = .data[["x"]],
                                label = .data[["case_id"]])))
  p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  p
}
