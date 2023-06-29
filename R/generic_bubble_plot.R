#' @title TBD
#'
#' @description TBD
#'
#' @details TBD
#'
#' @param influence_out The output from [influence_stat()].
#'
#' @param cutoff_gcd Cases with generalized Cook's distance or
#' approximate generalized Cook's distance larger
#' than this value will be labeled. Default is `NULL`. If `NULL`, no
#' cutoff line will be drawn.
#'
#' @param cutoff_md Cases with Mahalanobis distance larger than this
#' value will be labeled. If it is `TRUE`, the (`cutoff_md_qchisq` x
#' 100)th percentile of the chi-square distribution with the degrees
#' of freedom equal to the number of variables will be used.  Default
#' is `FALSE`, no cutoff value.
#'
#' @param cutoff_md_qchisq This value multiplied by 100 is the
#' percentile to be used for labeling case based on Mahalanobis
#' distance. Default is .975.
#'
#' @param largest_gcd The number of cases with the largest generalized
#' Cook's distance or approximate generalized Cook's distance
#' to be labelled. Default is 1. If not an integer, it
#' will be rounded to the nearest integer.
#'
#' @param largest_md  The number of cases with the largest Mahalanobis
#' distance to be labelled. Default is 1. If not an integer, it will
#' be rounded to the nearest integer.
#'
#' @param largest_fit_measure  The number of cases with the largest
#' selected fit measure change in magnitude to be labelled. Default is
#' 1. If not an integer, it will be rounded to the nearest integer.
#'
#' @param fit_measure The fit measure to be used in a
#' plot. Use the name in the [lavaan::fitMeasures()] function. No
#' default value.
#'
#' @param cutoff_fit_measure Cases with `fit_measure` larger than
#' this cutoff in magnitude will be labeled. No default value and
#' must be specified.
#'
#' @param circle_size The size of the largest circle when the size
#' of a circle is controlled by a statistic.
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
#' [ggplot2::geom_vline()] or
#' [ggplot2::geom_hline()] to modify how
#' to draw the line for user cutoff
#' value. Default is `list()`
#' and internal default settings will be
#' used.
#'
#' @param cutoff_line_gcd_aes Similar
#' to `cutoff_line_aes` but control
#' the line for the cutoff value of
#' *gCD*.
#'
#' @param cutoff_line_fit_measures_aes
#' Similar
#' to `cutoff_line_aes` but control
#' the line for the cutoff value of
#' the selected fit measure.
#'
#' @param cutoff_line_md_aes
#' Similar
#' to `cutoff_line_aes` but control
#' the line for the cutoff value of
#' the Mahalanobis distance.
#'
#' @param case_label_aes A named list of
#' arguments to be passed to
#' [ggrepel::geom_label_repel()] to
#' modify how to draw the labels for
#' cases marked (based on arguments
#' such as `cutoff_gcd` or `largest_gcd`).
#' Default is `list()` and internal
#' default settings will be used.
#'
#' @return A [ggplot2] plot. Plotted by default. If assigned to a variable
#' or called inside a function, it will not be plotted. Use [plot()] to
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
#' # Fit the model n times. Each time with one case removed.
#' # For illustration, do this only for selected cases.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Get all default influence stats
#' out <- influence_stat(fit_rerun)
#' head(out)
#'
#' # Plot generalized Cook's distance. Label the 3 cases with the largest distances.
#' gcd_plot(out, largest_gcd = 3)
#'
#' # Plot Mahalanobis distance. Label the 3 cases with the largest distances.
#' md_plot(out, largest_md = 3)
#'
#' # Plot case influence on model chi-square against generalized Cook's distance.
#' # Label the 3 cases with the largest absolute influence.
#' # Label the 3 cases with the largest generalized Cook's distance.
#' gcd_gof_plot(out, fit_measure = "chisq", largest_gcd = 3,
#'              largest_fit_measure = 3)
#'
#' # Plot case influence on model chi-square against Mahalanobis distance.
#' # Size of bubble determined by generalized Cook's distance.
#' # Label the 3 cases with the largest absolute influence.
#' # Label the 3 cases with the largest Mahalanobis distance.
#' # Label the 3 cases with the largest generalized Cook's distance.
#'
#' gcd_gof_md_plot(out, fit_measure = "chisq",
#'                      largest_gcd = 3,
#'                      largest_fit_measure = 3,
#'                      largest_md = 3,
#'                      circle_size = 10)
#'
#' # Use the approximate method that does not require refitting the model.
#'
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#' out <- influence_stat(fit)
#' head(out)
#'
#' # Plot approximate generalized Cook's distance.
#' # Label the 3 cases with the largest values.
#' gcd_plot(out, largest_gcd = 3)
#'
#' # Plot Mahalanobis distance.
#' # Label the 3 cases with the largest values.
#' md_plot(out, largest_md = 3)
#'
#' # Plot approximate case influence on model chi-square against
#' # approximate generalized Cook's distance.
#' # Label the 3 cases with the largest absolute approximate case influence.
#' # Label the 3 cases with the largest approximate generalized Cook's distance.
#' gcd_gof_plot(out, fit_measure = "chisq", largest_gcd = 3,
#'              largest_fit_measure = 3)
#'
#' # Plot approximate case influence on model chi-square against Mahalanobis distance.
#' # The size of a bubble determined by approximate generalized Cook's distance.
#' # Label the 3 cases with the largest absolute approximate case influence.
#' # Label the 3 cases with the largest Mahalanobis distance.
#' # Label the 3 cases with the largest approximate generalized Cook's distance.
#'
#' gcd_gof_md_plot(out, fit_measure = "chisq",
#'                      largest_gcd = 3,
#'                      largest_fit_measure = 3,
#'                      largest_md = 3,
#'                      circle_size = 10)
#'
#' # Customize elements in the plot.
#' # For example, change the color and shape of the points.
#'
#' gcd_gof_plot(out, fit_measure = "chisq", largest_gcd = 3,
#'              largest_fit_measure = 3,
#'              point_aes = list(shape = 3, color = "red"))
#'
#' @references Pek, J., & MacCallum, R. (2011). Sensitivity analysis
#'  in structural equation models: Cases and their influence.
#'  *Multivariate Behavioral Research, 46*(2), 202-228.
#'  doi:10.1080/00273171.2011.561068
#'
#' @seealso [influence_stat()].
#' @name influence_plot

#' @export

bubble_plot <- function(object,
                        x,
                        y,
                        z = NULL,
                        x_label = "X",
                        y_label = "Y",
                        z_label = "Z",
                        cutoff_x = NULL,
                        cutoff_y = NULL,
                        cutoff_z = NULL,
                        circle_size = 2,
                        largest_x = 1,
                        largest_y = 1,
                        largest_z = 1,
                        hline_at = 0,
                        point_aes = list(),
                        hline_aes = list(),
                        cutoff_line_x_aes = list(),
                        cutoff_line_y_aes = list(),
                        cutoff_line_z_aes = list(),
                        case_label_aes = list()
                        ) {
  if (length(dim(object)) != 2) {
      stop("'object' invalid. Must be a matrix-like object.")
    }
  x <- as.character(x)
  if (!(x %in% colnames(object))) {
      stop(sQuote(x), " not found in 'object'.")
    }
  y <- as.character(y)
  if (!(y %in% colnames(object))) {
      stop(sQuote(y), " not found in 'object'.")
    }
  if (!is.null(z)) {
      z <- as.character(z)
      if (!(y %in% colnames(object))) {
          stop(sQuote(z), " not found in 'object'.")
        }
    }
  object <- object[, c(x, y, z), drop = TRUE]

  case_ids <- rownames(object)
  row_id   <- seq_len(nrow(object))
  dat <- data.frame(row_id = row_id,
                    case_id = case_ids,
                    object,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)

  # method <- attr(influence_out, "method")
  # if (method == "approx") {
  #     dat$gcd <- dat$gcd_approx
  #     gcd_label <- "Approximate Generalized Cook's Distance"
  #     gcd_label_short <- "Approx. gCD"
  #     change_label <- "Approximate Change in Fit Measure"
  #   } else {
  #     gcd_label <- "Generalized Cook's Distance"
  #     gcd_label_short <- "gCD"
  #     change_label <- "Change in Fit Measure"
  #   }

  point_aes <- utils::modifyList(list(shape = 21,
                                      alpha = .50,
                                      fill = "white"),
                                 point_aes)
  # The following part should never be changed by users.
  if (!is.null(z)) {
      point_aes <- utils::modifyList(point_aes,
                            list(mapping = ggplot2::aes(size = .data[[z]])))
    }

  hline_aes <- utils::modifyList(list(linetype = "solid"),
                                  hline_aes)
  # The following part should never be changed by users.
  hline_aes <- utils::modifyList(hline_aes,
                                  list(yintercept = hline_at))

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data[[x]], .data[[y]]))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + do.call(ggplot2::geom_hline, hline_aes)
  tmp <- paste0(y_label, " against ", x_label, "\n",
                ifelse(is.null(z), "", paste0(z_label, " as the Size")))
  p <- p + ggplot2::scale_size_area(name = z_label,
                                  max_size = circle_size) +
         ggplot2::labs(title = tmp) +
         ggplot2::xlab(x_label) +
         ggplot2::ylab(y_label)

  if (is.numeric(cutoff_y)) {
      cutoff_line_y_aes <- utils::modifyList(list(linetype = "dashed"),
                                    cutoff_line_y_aes)
      # The following part should never be changed by users.
      cutoff_line_y_aes1 <- utils::modifyList(cutoff_line_y_aes,
                                    list(yintercept = cutoff_y))
      cutoff_line_y_aes2 <- utils::modifyList(cutoff_line_y_aes,
                                    list(yintercept = -cutoff_y))
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_y_aes1)
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_y_aes2)
      c_y_cut <- abs(cutoff_y)
    } else {
      c_y_cut <- Inf
    }
  if (is.numeric(largest_y) && largest_y >= 1) {
      m_y <- round(largest_y)
      o_y <- order(abs(dat[, y, drop = TRUE]), decreasing = TRUE)
      m_y_cut <- abs(dat[o_y[m_y], y, drop = TRUE])
    } else {
      m_y_cut <- Inf
    }
  label_y <- (abs(dat[, y, drop = TRUE]) >= c_y_cut) |
             (abs(dat[, y, drop = TRUE]) >= m_y_cut)

  # k <- ncol(fit0@Data@X[[1]])
  c_x_cut <- Inf
  # if (isTRUE(cutoff_md)) {
  #     c_md_cut <- stats::qchisq(cutoff_md_qchisq, k)
  #   }
  # if (is.numeric(cutoff_md)) {
  #     c_md_cut <- cutoff_md
  #   }
  if (is.numeric(largest_x) && largest_x >= 1) {
      m_x <- round(largest_x)
      o_x <- order(abs(dat[, x, drop = TRUE]), decreasing = TRUE)
      m_x_cut <- abs(dat[o_x[m_x], x, drop = TRUE])
    } else {
      m_x_cut <- Inf
    }
  if (is.numeric(c_x_cut) && c_x_cut < Inf) {
      cutoff_line_x_aes <- utils::modifyList(list(linetype = "dashed"),
                                    cutoff_line_x_aes)
      # The following part should never be changed by users.
      cutoff_line_x_aes1 <- utils::modifyList(cutoff_line_x_aes,
                                    list(xintercept = cutoff_x))
      cutoff_line_x_aes2 <- utils::modifyList(cutoff_line_x_aes,
                                    list(xintercept = -cutoff_x))
      p <- p + do.call(ggplot2::geom_vline, cutoff_line_x_aes1)
      p <- p + do.call(ggplot2::geom_vline, cutoff_line_x_aes2)
      c_x_cut <- abs(cutoff_x)
    }
  label_x <- (abs(dat[, x, drop = TRUE]) >= c_x_cut) |
             (abs(dat[, x, drop = TRUE]) >= m_x_cut)

  if (is.numeric(cutoff_z)) {
      c_z_cut <- cutoff_z
    } else {
      c_z_cut <- Inf
    }
  if (!is.null(z)) {
      if (is.numeric(largest_z) && largest_z >= 1) {
          m_z <- round(largest_z)
          o_z <- order(dat$gcd, decreasing = TRUE)
          m_z_cut <- abs(dat[o_z[m_z], z, drop = TRUE])
        } else {
          m_z_cut <- Inf
        }
      label_z <- (abs(dat[, z, drop = TRUE]) >= c_z_cut) |
                (abs(dat[, z, drop = TRUE]) >= m_z_cut)
    } else {
      m_z_cut <- Inf
      label_z <- rep(FALSE, nrow(object))
    }

  case_label_aes <- utils::modifyList(list(),
                                  case_label_aes)
  # The following part should never be changed by users.
  case_label_aes <- utils::modifyList(case_label_aes,
                        list(data = dat[label_x | label_y | label_z, ],
                             mapping = ggplot2::aes(
                                x = .data[[x]],
                                y = .data[[y]],
                                label = .data[["case_id"]])))
  p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  p
}

#' @noRd

gcd_plot <- function(influence_out,
                     cutoff_gcd = NULL,
                     largest_gcd = 1,
                     point_aes = list(),
                     vline_aes = list(),
                     cutoff_line_aes = list(),
                     case_label_aes = list()
                     ) {
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
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

  case_ids <- rownames(influence_out)
  row_id   <- seq_len(nrow(influence_out))
  dat <- data.frame(row_id = row_id,
                    case_id = case_ids,
                    influence_out,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  method <- attr(influence_out, "method")
  if (method == "approx") {
      dat$gcd <- dat$gcd_approx
      gcd_label <- "Approximate Generalized Cook's Distance"
    } else {
      gcd_label <- "Generalized Cook's Distance"
    }
  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$row_id, .data$gcd))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + ggplot2::labs(title = gcd_label)
  p <- p + do.call(ggplot2::geom_segment, vline_aes)
  p <- p + ggplot2::xlab("Row Number") +
         ggplot2::ylab(gcd_label)

  if (is.numeric(cutoff_gcd)) {
      cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"),
                                                cutoff_line_aes)
      # The following part should never be changed by users.
      cutoff_line_aes <- utils::modifyList(cutoff_line_aes,
                                    list(yintercept = cutoff_gcd))
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes)
      c_gcd_cut <- cutoff_gcd
    } else {
      c_gcd_cut <- Inf
    }
  if (is.numeric(largest_gcd) && largest_gcd >= 1) {
      m_gcd <- round(largest_gcd)
      o_gcd <- order(dat$gcd, decreasing = TRUE)
      m_gcd_cut <- dat$gcd[o_gcd[m_gcd]]
    } else {
      m_gcd_cut <- Inf
    }
  label_gcd <- (dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut)

  case_label_aes <- utils::modifyList(list(position = ggplot2::position_dodge(.5)),
                                  case_label_aes)
  # The following part should never be changed by users.
  case_label_aes <- utils::modifyList(case_label_aes,
                        list(data = dat[label_gcd, ],
                             mapping = ggplot2::aes(
                                x = .data[["row_id"]],
                                y = .data[["gcd"]],
                                label = .data[["case_id"]])))
  p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  p
}

#' @noRd

md_plot_tmp2 <- function(influence_out,
                    cutoff_md = FALSE,
                    cutoff_md_qchisq = .975,
                    largest_md = 1,
                    point_aes = list(),
                    vline_aes = list(),
                    cutoff_line_aes = list(),
                    case_label_aes = list()
                    ) {
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
    }
  if (!inherits(attr(influence_out, "fit"), "lavaan")) {
      stop(paste("The original lavaan output is not in the attributes.",
                 "Was subsetting used to get influence_out?"))
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

  fit0 <- attr(influence_out, "fit")
  case_ids <- rownames(influence_out)
  row_id   <- seq_len(nrow(influence_out))
  dat <- data.frame(row_id = row_id,
                    case_id = case_ids,
                    influence_out,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  if (all(is.na(dat$md))) {
      stop("All cases have no value on Mahalanobis distance (md).")
    }

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$row_id, .data$md))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + do.call(ggplot2::geom_segment, vline_aes)
  p <- p + ggplot2::labs(title = "Mahalanobis Distance") +
         ggplot2::xlab("Row Number") +
         ggplot2::ylab("Mahalanobis Distance")

  k <- ncol(fit0@Data@X[[1]])
  c_md_cut <- Inf
  if (isTRUE(cutoff_md)) {
      c_md_cut <- stats::qchisq(cutoff_md_qchisq, k)
    }
  if (is.numeric(cutoff_md)) {
      c_md_cut <- cutoff_md
    }
  if (is.numeric(largest_md) && largest_md >= 1) {
      m_md <- round(largest_md)
      o_md <- order(dat$md, decreasing = TRUE)
      m_md_cut <- dat$md[o_md[m_md]]
    } else {
      m_md_cut <- Inf
    }
  if (is.numeric(c_md_cut) && c_md_cut < Inf) {
      cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"),
                                                cutoff_line_aes)
      # The following part should never be changed by users.
      cutoff_line_aes <- utils::modifyList(cutoff_line_aes,
                                    list(yintercept = c_md_cut))
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes)
    }
  label_md <- (dat$md >= c_md_cut) | (dat$md >= m_md_cut)

  case_label_aes <- utils::modifyList(list(position = ggplot2::position_dodge(.5)),
                                  case_label_aes)
  # The following part should never be changed by users.
  case_label_aes <- utils::modifyList(case_label_aes,
                        list(data = dat[label_md, ],
                             mapping = ggplot2::aes(
                                x = .data[["row_id"]],
                                y = .data[["md"]],
                                label = .data[["case_id"]])))
  p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  p
}

#' @noRd

gcd_gof_plot_tmp <- function(influence_out,
                         fit_measure,
                         cutoff_gcd = NULL,
                         cutoff_fit_measure = NULL,
                         largest_gcd = 1,
                         largest_fit_measure = 1,
                         point_aes = list(),
                         hline_aes = list(),
                         cutoff_line_gcd_aes = list(),
                         cutoff_line_fit_measures_aes = list(),
                         case_label_aes = list()
                         ) {
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
    }
  if (missing(fit_measure)) {
      stop("No fit_measure is selected.")
    }
  case_ids <- rownames(influence_out)
  row_id   <- seq_len(nrow(influence_out))
  dat <- data.frame(row_id = row_id,
                    case_id = case_ids,
                    influence_out,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  dat$fm <- dat[, fit_measure]

  method <- attr(influence_out, "method")
  if (method == "approx") {
      dat$gcd <- dat$gcd_approx
      gcd_label <- "Approximate Generalized Cook's Distance"
      change_label <- "Approximate Change in Fit Measure"
    } else {
      gcd_label <- "Generalized Cook's Distance"
      change_label <- "Change in Fit Measure"
    }

  point_aes <- utils::modifyList(list(),
                                 point_aes)

  hline_aes <- utils::modifyList(list(linetype = "solid"),
                                  hline_aes)
  # The following part should never be changed by users.
  hline_aes <- utils::modifyList(hline_aes,
                                  list(yintercept = 0))

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$gcd, .data$fm))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + ggplot2::labs(title =
            paste0(change_label, " against\n", gcd_label))
  p <- p + do.call(ggplot2::geom_hline, hline_aes)
  p <- p + ggplot2::xlab(gcd_label) +
           ggplot2::ylab(change_label)

  if (is.numeric(cutoff_fit_measure)) {
      cutoff_line_fit_measures_aes <- utils::modifyList(list(linetype = "dashed"),
                                    cutoff_line_fit_measures_aes)
      # The following part should never be changed by users.
      cutoff_line_fit_measures_aes1 <- utils::modifyList(cutoff_line_fit_measures_aes,
                                    list(yintercept = cutoff_fit_measure))
      cutoff_line_fit_measures_aes2 <- utils::modifyList(cutoff_line_fit_measures_aes,
                                    list(yintercept = -cutoff_fit_measure))
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_fit_measures_aes1)
      p <- p + do.call(ggplot2::geom_hline, cutoff_line_fit_measures_aes2)
      c_fm_cut <- abs(cutoff_fit_measure)
    } else {
      c_fm_cut <- Inf
    }
  if (is.numeric(largest_fit_measure) && largest_fit_measure >= 1) {
      m_fm <- round(largest_fit_measure)
      o_fm <- order(abs(dat$fm), decreasing = TRUE)
      m_fm_cut <- abs(dat$fm[o_fm[m_fm]])
    } else {
      m_fm_cut <- Inf
    }
  label_fm <- (abs(dat$fm) >= c_fm_cut) | (abs(dat$fm) >= m_fm_cut)

  if (is.numeric(cutoff_gcd)) {
      cutoff_line_gcd_aes <- utils::modifyList(list(linetype = "dashed"),
                                    cutoff_line_gcd_aes)
      # The following part should never be changed by users.
      cutoff_line_gcd_aes <- utils::modifyList(cutoff_line_gcd_aes,
                                    list(xintercept = cutoff_gcd))
      p <- p + do.call(ggplot2::geom_vline, cutoff_line_gcd_aes)
      c_gcd_cut <- cutoff_gcd
    } else {
      c_gcd_cut <- Inf
    }
  if (is.numeric(largest_gcd) && largest_gcd >= 1) {
      m_gcd <- round(largest_gcd)
      o_gcd <- order(dat$gcd, decreasing = TRUE)
      m_gcd_cut <- dat$gcd[o_gcd[m_gcd]]
    } else {
      m_gcd_cut <- Inf
    }
  label_gcd <- (dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut)

  case_label_aes <- utils::modifyList(list(),
                                  case_label_aes)
  # The following part should never be changed by users.
  case_label_aes <- utils::modifyList(case_label_aes,
                        list(data = dat[label_gcd | label_fm, ],
                             mapping = ggplot2::aes(
                                x = .data[["gcd"]],
                                y = .data[["fm"]],
                                label = .data[["case_id"]])))
  p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)

  p
}

