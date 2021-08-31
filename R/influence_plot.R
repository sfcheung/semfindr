#' @title
#' Plot selected influence statistics and measures of extremeness
#'
#' @description
#' Get a [influence_stat()] output and plot selected statistics
#'
#' @details
#' The output of [influence_stat()] is simply a matrix. Therefore, this
#' function will work for any matrix provided.
#' Row number will be used on the x-axis if applicable. However, case
#' identification
#' values in the output from [influence_stat()] will be used for labeling
#' individual cases.
#'
#' @param influence_out The output from [influence_stat()].
#' @param cutoff_gcd Cases with generalized Cook's distance larger than this
#'                  value will be labeled. Default is `NULL`. If `NULL`, no
#'                  cutoff line will be drawn.
#' @param cutoff_md Cases with Mahalanobis distance larger than this value
#'                  will be labeled. If it is `TRUE`,
#'                  the (`cutoff_md_qchisq` x 100)th percentile of the
#'                  chi-square distribution with
#'                  the degrees of freedom equal to the number of variables
#'                  will be used.  Default is `FALSE`, no cutoff value.
#' @param cutoff_md_qchisq This value multiplied by 100 is the percentile to
#'                         be used
#'                         for labeling case based on Mahalanobis distance.
#'                         Default is .975.
#' @param largest_gcd The number of cases with the largest generalized
#'                    Cook's distance
#'                    to be labelled.
#'                   Default is 1. If not an integer, it will be rounded
#'                    to the nearest
#'                   integer.
#' @param largest_md  The number of cases with the largest Mahalanobis distance
#'                     to be labelled.
#'                   Default is 1. If not an integer, it will be rounded to
#'                   the nearest
#'                   integer.
#' @param largest_fit_measure  The number of cases with the largest selected
#'                    fit measure change in magnitude
#'                   to be labelled.
#'                   Default is 1. If not an integer, it will be rounded to
#'                   the nearest
#'                   integer.
#' @param fit_measure Specify the fit measure to be used in a plot. Use the
#'                    name
#'                    in the [lavaan::fitMeasures()] function. No default
#'                    value.
#' @param cutoff_fit_measure Cases with `fit_measure` larger than this cutoff
#'                           in magnitude
#'                          will be labeled. No default value and must be
#'                           specified.
#' @param circle_size The size of the largest circle when the size of a circle
#'                     is controlled
#'                   by a statistic.
#'
#' @return
#' A [ggplot2] plot. It will not be plotted. To plot it, use [plot()] on
#'  the output.
#'
#' @author S. F. Cheung (shufai.cheung@gmail.com)
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration only, select only the first 50 cases
#' dat <- dat[1:50, ]
#' # The model
#' mod <-
#' "
#' m1 ~ iv1 + iv2
#' dv ~ m1
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#' # Fit the model n times. Each time with one case removed.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#' # Get all default influence stats
#' out <- influence_stat(fit_rerun)
#' head(out)
#'
#' # Plot generalized Cook's distance. Label the 3 cases with largest distances.
#' gcd_plot(out, largest_gcd = 3)
#'
#' # Plot Mahalanobis distance. Label the 3 cases with largest distances.
#' md_plot(out, largest_md = 3)
#'
#' # Plot changes in model chi-square against generalized Cook's distance.
#' # Label the 3 cases largest changes in magnitude.
#' # Label the 3 cases with largest generalized Cook's distance.
#' gcd_gof_plot(out, fit_measure = "chisq", largest_gcd = 3,
#'              largest_fit_measure = 3)
#'
#' # Plot changes in model chi-square against Mahalanobis distance.
#' # Size of bubble determined by generalized Cook's distance.
#' # Label the 3 cases largest changes in magnitude.
#' # Label the 3 cases with largest Mahalanobis distance.
#' # Label the 3 cases with largest generalized Cook's distance.
#' #
#' gcd_gof_md_plot(out, fit_measure = "chisq",
#'                      largest_gcd = 3,
#'                      largest_fit_measure = 3,
#'                      largest_md = 3,
#'                      circle_size = 10)
#'
#'
#' @references
#' Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation
#'  models: Cases and their influence. *Multivariate Behavioral Research,
#'  46*(2), 202-228. <https://doi.org/10.1080/00273171.2011.561068>
#'
#' @seealso [influence_stat()].
#' @name influence_plot
NULL

#' @importFrom rlang .data
#' @rdname influence_plot
#' @export

gcd_plot <- function(
                       influence_out,
                       cutoff_gcd = NULL,
                       largest_gcd = 1
                       ) {
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
    }
  case_ids <- rownames(influence_out)
  row_id   <- seq_len(nrow(influence_out))
  dat <- data.frame(row_id = row_id,
                    case_id = case_ids,
                    influence_out,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$row_id, .data$gcd)) +
         ggplot2::geom_point() +
         ggplot2::labs(title = "Generalized Cook's Distance") +
         ggplot2::geom_segment(
                    ggplot2::aes(xend = .data$row_id,
                                 yend = 0),
                                 size = 1,
                                 lineend = "butt") +
         ggplot2::xlab("Row Number") +
         ggplot2::ylab("Generalized Cook's Distance")

  if (is.numeric(cutoff_gcd)) {
      p <- p + ggplot2::geom_hline(yintercept = cutoff_gcd,
                                   linetype = "dashed")
      c_gcd_cut <- cutoff_gcd
    } else {
      c_gcd_cut <- Inf
    }
  if (is.numeric(largest_gcd) & largest_gcd >= 1) {
      m_gcd <- round(largest_gcd)
      o_gcd <- order(dat$gcd, decreasing = TRUE)
      m_gcd_cut <- dat$gcd[o_gcd[m_gcd]]
    } else {
      m_gcd_cut <- Inf
    }
  label_gcd <- (dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut)
  p <- p + ggrepel::geom_label_repel(
              data = dat[label_gcd, ],
              ggplot2::aes(.data$row_id, .data$gcd, label = .data$case_id),
              position = ggplot2::position_dodge(.5))
  p
}

#' @importFrom rlang .data
#' @rdname influence_plot
#' @export

md_plot <- function(
                       influence_out,
                       cutoff_md = FALSE,
                       cutoff_md_qchisq = .975,
                       largest_md = 1
                       ) {
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
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

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$row_id, .data$md)) +
         ggplot2::geom_point() +
         ggplot2::geom_segment(
                    ggplot2::aes(xend = .data$row_id,
                                 yend = 0),
                                 size = 1,
                                 lineend = "butt") +
         ggplot2::labs(title = "Mahalanobis Distance") +
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
  if (is.numeric(largest_md) & largest_md >= 1) {
      m_md <- round(largest_md)
      o_md <- order(dat$md, decreasing = TRUE)
      m_md_cut <- dat$md[o_md[m_md]]
    } else {
      m_md_cut <- Inf
    }
  if (is.numeric(c_md_cut) & c_md_cut < Inf) {
    p <- p + ggplot2::geom_hline(yintercept = c_md_cut,
                             linetype = "dashed")
    }
  label_md <- (dat$md >= c_md_cut) | (dat$md >= m_md_cut)
  p <- p + ggrepel::geom_label_repel(
              data = dat[label_md, ],
              ggplot2::aes(.data$row_id, .data$md, label = .data$case_id),
              position = ggplot2::position_dodge(.5))
  p
}

#' @importFrom rlang .data
#' @rdname influence_plot
#' @export

gcd_gof_plot <- function(
                       influence_out,
                       fit_measure,
                       cutoff_gcd = NULL,
                       cutoff_fit_measure = NULL,
                       largest_gcd = 1,
                       largest_fit_measure = 1
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

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$gcd, .data$fm)) +
         ggplot2::geom_point() +
         ggplot2::labs(title =
            "Change in Test Statistics against Generalized Cook's Distance") +
         ggplot2::geom_hline(yintercept = 0,
                             linetype = "solid") +
         ggplot2::xlab("Generalized Cook's Distance") +
         ggplot2::ylab("Change in Test Statistics")

  if (is.numeric(cutoff_fit_measure)) {
      p <- p +  ggplot2::geom_hline(yintercept = cutoff_fit_measure,
                               linetype = "dashed") +
                ggplot2::geom_hline(yintercept = -1 * cutoff_fit_measure,
                                 linetype = "dashed")
      c_fm_cut <- abs(cutoff_fit_measure)
    } else {
      c_fm_cut <- Inf
    }
  if (is.numeric(largest_fit_measure) & largest_fit_measure >= 1) {
      m_fm <- round(largest_fit_measure)
      o_fm <- order(abs(dat$fm), decreasing = TRUE)
      m_fm_cut <- abs(dat$fm[o_fm[m_fm]])
    } else {
      m_fm_cut <- Inf
    }
  label_fm <- (abs(dat$fm) >= c_fm_cut) | (abs(dat$fm) >= m_fm_cut)

  if (is.numeric(cutoff_gcd)) {
      p <- p + ggplot2::geom_hline(yintercept = cutoff_gcd,
                                   linetype = "dashed")
      c_gcd_cut <- cutoff_gcd
    } else {
      c_gcd_cut <- Inf
    }
  if (is.numeric(largest_gcd) & largest_gcd >= 1) {
      m_gcd <- round(largest_gcd)
      o_gcd <- order(dat$gcd, decreasing = TRUE)
      m_gcd_cut <- dat$gcd[o_gcd[m_gcd]]
    } else {
      m_gcd_cut <- Inf
    }
  label_gcd <- (dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut)

  p <- p + ggrepel::geom_label_repel(
              data = dat[label_gcd | label_fm, ],
              ggplot2::aes(.data$gcd, .data$fm, label = .data$case_id))
  p
}

#' @importFrom rlang .data
#' @rdname influence_plot
#' @export

gcd_gof_md_plot <- function(
                       influence_out,
                       fit_measure,
                       cutoff_md = FALSE,
                       cutoff_fit_measure = NULL,
                       circle_size = 2,
                       cutoff_md_qchisq = .975,
                       cutoff_gcd = NULL,
                       largest_gcd = 1,
                       largest_md = 1,
                       largest_fit_measure = 1
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

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$md, .data$fm)) +
         ggplot2::geom_point(ggplot2::aes(size = .data$gcd),
                             shape = 21,
                             alpha = .50,
                             fill = "white") +
         ggplot2::scale_size_area(name = "gCD", max_size = circle_size) +
         ggplot2::labs(title =
            paste0("Change in Test Statistics against Mahalanobis Distance,\n",
                   "Generalized Cook's Distance as the Size")) +
         ggplot2::xlab("Mahalanobis Distance") +
         ggplot2::ylab("Change in Test Statistics")

  if (is.numeric(cutoff_fit_measure)) {
      p <- p +  ggplot2::geom_hline(yintercept = cutoff_fit_measure,
                               linetype = "dashed") +
                ggplot2::geom_hline(yintercept = -1 * cutoff_fit_measure,
                                 linetype = "dashed")
      c_fm_cut <- abs(cutoff_fit_measure)
    } else {
      c_fm_cut <- Inf
    }
  if (is.numeric(largest_fit_measure) & largest_fit_measure >= 1) {
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
  if (is.numeric(largest_md) & largest_md >= 1) {
      m_md <- round(largest_md)
      o_md <- order(dat$md, decreasing = TRUE)
      m_md_cut <- dat$md[o_md[m_md]]
    } else {
      m_md_cut <- Inf
    }
  if (is.numeric(c_md_cut) & c_md_cut < Inf) {
  p <- p + ggplot2::geom_vline(xintercept = c_md_cut,
                             linetype = "dashed")
    }
  label_md <- (dat$md >= c_md_cut) | (dat$md >= m_md_cut)

  if (is.numeric(cutoff_gcd)) {
      p <- p + ggplot2::geom_hline(yintercept = cutoff_gcd,
                                   linetype = "dashed")
      c_gcd_cut <- cutoff_gcd
    } else {
      c_gcd_cut <- Inf
    }
  if (is.numeric(largest_gcd) & largest_gcd >= 1) {
      m_gcd <- round(largest_gcd)
      o_gcd <- order(dat$gcd, decreasing = TRUE)
      m_gcd_cut <- dat$gcd[o_gcd[m_gcd]]
    } else {
      m_gcd_cut <- Inf
    }
  label_gcd <- (dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut)

  p <- p + ggrepel::geom_label_repel(
              data = dat[label_fm | label_md | label_gcd, ],
              ggplot2::aes(.data$md, .data$fm, label = .data$case_id))

  p
}