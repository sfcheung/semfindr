#' @title Plots of Influence Measures
#'
#' @description Gets an [influence_stat()] output and plots selected
#' statistics.
#'
#' @details The output of [influence_stat()] is simply a matrix.
#' Therefore, these functions will work for any matrix provided. Row
#' number will be used on the x-axis if applicable. However, case
#' identification values in the output from [influence_stat()] will
#' be used for labeling individual cases.
#'
#' The default settings for the plots
#' should be good enough for diagnostic
#' purpose. If so desired, users can
#' use the `*_aes` arguments to nearly
#' fully customize all the major
#' elements of the plots, as they would
#' do for building a [ggplot2] plot.
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
NULL

#' @importFrom rlang .data
#' @describeIn influence_plot Index plot of generalized Cook's distance.
#' @export

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
  dat <- dat[!is.na(dat$gcd), ]
  if (nrow(dat) == 0) {
      stop("All cases have gCD missing.")
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

#' @importFrom rlang .data
#' @describeIn influence_plot Index plot of Mahalanobis distance.
#' @export

md_plot <- function(influence_out,
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

#' @importFrom rlang .data
#' @describeIn influence_plot Plot the case influence of the selected fit
#'  measure against generalized Cook's distance.
#' @export

gcd_gof_plot <- function(influence_out,
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
  dat <- dat[!is.na(dat$gcd) &
             !is.na(dat$fm), ]
  if (nrow(dat) == 0) {
      stop("No cases have non-missing values.")
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

#' @importFrom rlang .data
#' @describeIn influence_plot Bubble plot of the case influence of the selected
#'  fit measure against Mahalanobis distance, with the size of a bubble
#'  determined by generalized Cook's distance.
#' @export

gcd_gof_md_plot <- function(influence_out,
                            fit_measure,
                            cutoff_md = FALSE,
                            cutoff_fit_measure = NULL,
                            circle_size = 2,
                            cutoff_md_qchisq = .975,
                            cutoff_gcd = NULL,
                            largest_gcd = 1,
                            largest_md = 1,
                            largest_fit_measure = 1,
                            point_aes = list(),
                            hline_aes = list(),
                            cutoff_line_md_aes = list(),
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
  if (!inherits(attr(influence_out, "fit"), "lavaan")) {
      stop(paste("The original lavaan output is not in the attributes.",
                 "Was subsetting used to get influence_out?"))
    }
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
  dat$fm <- dat[, fit_measure]

  method <- attr(influence_out, "method")
  if (method == "approx") {
      dat$gcd <- dat$gcd_approx
      gcd_label <- "Approximate Generalized Cook's Distance"
      gcd_label_short <- "Approx. gCD"
      change_label <- "Approximate Change in Fit Measure"
    } else {
      gcd_label <- "Generalized Cook's Distance"
      gcd_label_short <- "gCD"
      change_label <- "Change in Fit Measure"
    }
  dat <- dat[!is.na(dat$gcd) &
             !is.na(dat$fm) &
             !is.na(dat$md), ]
  if (nrow(dat) == 0) {
      stop("No cases have valid values.")
    }

  point_aes <- utils::modifyList(list(shape = 21,
                                      alpha = .50,
                                      fill = "white"),
                                 point_aes)
  # The following part should never be changed by users.
  point_aes <- utils::modifyList(point_aes,
                        list(mapping = ggplot2::aes(size = .data[["gcd"]])))

  hline_aes <- utils::modifyList(list(linetype = "solid"),
                                  hline_aes)
  # The following part should never be changed by users.
  hline_aes <- utils::modifyList(hline_aes,
                                  list(yintercept = 0))

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$md, .data$fm))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + do.call(ggplot2::geom_hline, hline_aes)
  p <- p + ggplot2::scale_size_area(name = gcd_label_short,
                                  max_size = circle_size) +
         ggplot2::labs(title =
            paste0(change_label, " against Mahalanobis Distance,\n",
                   gcd_label, " as the Size")) +
         ggplot2::xlab("Mahalanobis Distance") +
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
      cutoff_line_md_aes <- utils::modifyList(list(linetype = "dashed"),
                                    cutoff_line_md_aes)
      # The following part should never be changed by users.
      cutoff_line_md_aes <- utils::modifyList(cutoff_line_md_aes,
                                    list(xintercept = c_md_cut))
      p <- p + do.call(ggplot2::geom_vline, cutoff_line_md_aes)
    }
  label_md <- (dat$md >= c_md_cut) | (dat$md >= m_md_cut)

  if (is.numeric(cutoff_gcd)) {
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
                        list(data = dat[label_fm | label_md | label_gcd, ],
                             mapping = ggplot2::aes(
                                x = .data[["md"]],
                                y = .data[["fm"]],
                                label = .data[["case_id"]])))
  p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  p
}