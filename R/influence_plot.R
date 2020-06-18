#'@title
#' Plot selected influence statistics and measures of extremeness
#'
#'@description
#' Get a [influence_stat()] output and plot selected statistics
#'
#'@details
#' The output of [influence_stat()] is simply a matrix. Therefore, this 
#' function will work for any matrix provided. 
#' Row number will be used on the x-axis if applicable. However, case identification
#' values in the output from [influence_stat()] will be used for labeling 
#' individual cases.
#'
#'@param influence_out The output from [influence_stat()].
#'@param cutoff_gcd Cases with generalized Cook's distance larger than this 
#'                  value will be labeled. Default is `NULL`. If `NULL`, no 
#'                  cutoff line will be drawn.
#'@param cutoff_md Cases with Mahalanobis distance larger than this value
#'                  will be labeled. If it is `TRUE`,
#'                  the (`cutoff_md_qchisq` x 100)th percentile of the chi-square distribution with 
#'                  the degrees of freedom equal to the number of variables 
#'                  will be used.  Default is `FALSE`, no cutoff value.
#'@param cutoff_md_qchisq This value multiplied by 100 is the percentile to be used
#'                        for labeling case based on Mahalanobis distance. Default is .975.
#'@param largest_gcd The number of cases with the largest Cook's distance to be labelled. 
#'                   Default is 1. If not a integer, it will be rounded to the nearest
#'                   integer.
#'@param largest_md  The number of cases with the largest Mahalanobis distance to be labelled. 
#'                   Default is 1. If not a integer, it will be rounded to the nearest
#'                   integer.
#'@param fit_measure Specify the fit measure to be used in a plot. Use the name 
#'                   in the [lavaan::fitMeasures()] function. No default value.
#'@param cutoff_fit_measure Cases with `fit_measure` larger than this cutoff in magnitude
#'                          will be labeled. No default value and must be specified.
#'@param circle_size The size of the largest circle when the size of a circle is controlled
#'                   by a statistic.
#'
#'@return
#' A [ggplot2] plot. It will not be plot. To plot it, use [plot()] on the output.
#'
#'@examples
#'# To be prepared.
#'
#'@seealso [influence_stat()].
#'@name influence_plot
NULL

#'@importFrom rlang .data
#'@rdname influence_plot
#'@export

gcd_plot <- function(
                       influence_out,
                       cutoff_gcd = NULL,
                       largest_gcd = 1
                       )
{
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
  p <- p + ggrepel::geom_label_repel(
              data = dat[(dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut), ],
              ggplot2::aes(.data$row_id, .data$gcd, label = .data$case_id),
              position = ggplot2::position_dodge(.5))
  p       
}

#'@importFrom rlang .data
#'@rdname influence_plot
#'@export

md_plot <- function(
                       influence_out,
                       cutoff_md = FALSE,
                       cutoff_md_qchisq = .975,
                       largest_md = 1
                       )
{
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
    }
  if (!inherits(attr(influence_out, "fit"), "lavaan")) {
      stop("The original lavaan output is not in the attributes. Was subsetting used to get influence_out?")
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
  if (is.numeric(c_md_cut) & c_md_cut < Inf) {
    p <- p + ggplot2::geom_hline(yintercept = c_md_cut,
                             linetype = "dashed")
    }
  p <- p + ggrepel::geom_label_repel(
              data = dat[(dat$md >= c_md_cut) | (dat$md >= m_md_cut), ],
              ggplot2::aes(.data$row_id, .data$md, label = .data$case_id),
              position = ggplot2::position_dodge(.5))
  p       
}

#'@importFrom rlang .data
#'@rdname influence_plot
#'@export

gcd_gof_plot <- function(
                       influence_out,
                       fit_measure,
                       cutoff_gcd = NULL,
                       cutoff_fit_measure,
                       largest_gcd = 1
                       )
{
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
    }
  if (missing(fit_measure)) {
      stop("No fit_measure is selected.")
    }
  if (missing(cutoff_fit_measure)) {
      stop("No cutoff_fit_measure is specified.")
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
         ggplot2::labs(title = "Change in Test Statistics against Generalized Cook's Distance") + 
         ggplot2::geom_hline(yintercept = 0,
                             linetype = "solid") + 
         ggplot2::geom_hline(yintercept = cutoff_fit_measure,
                             linetype = "dashed") + 
         ggplot2::geom_hline(yintercept = -1*cutoff_fit_measure,
                             linetype = "dashed") + 
         ggplot2::xlab("Generalized Cook's Distance") + 
         ggplot2::ylab("Change in Test Statistics")
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
  p <- p + ggrepel::geom_label_repel(
              data = dat[(dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut), ],
              ggplot2::aes(.data$gcd, .data$fm, label = .data$case_id))
  p       
}

#'@importFrom rlang .data
#'@rdname influence_plot
#'@export

gcd_gof_md_plot <- function(
                       influence_out,
                       fit_measure,
                       cutoff_md = FALSE,
                       cutoff_fit_measure,
                       circle_size = 2,
                       cutoff_md_qchisq = .975,
                       cutoff_gcd = NULL,
                       largest_gcd = 1,
                       largest_md = 1
                       )
{
  if (missing(influence_out)) {
      stop("No influence_stat output supplied.")
    }
  if (missing(fit_measure)) {
      stop("No fit_measure is selected.")
    }
  if (missing(cutoff_fit_measure)) {
      stop("No cutoff_fit_measure is specified.")
    }
  if (!inherits(attr(influence_out, "fit"), "lavaan")) {
      stop("The original lavaan output is not in the attributes. Was subsetting used to get influence_out?")
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
  # if (is.null(cutoff_md)) {
      # cutoff_md <- stats::qchisq(cutoff_md_qchisq, k)
    # }
  #dat$gcd2 <- circle_size*dat$gcd/max(dat$gcd)
  
  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$md, .data$fm)) + 
#         ggplot2::geom_point(size = .05) + 
         ggplot2::geom_point(ggplot2::aes(size = .data$gcd),
                             shape = 21,
                             alpha = .50,
                             fill = "white") + 
         ggplot2::scale_size_area(name = "gCD", max_size = circle_size) + 
         ggplot2::labs(title = "Change in Test Statistics against Mahalanobis Distance, with Generalized Cook's Distance as size") + 
         ggplot2::geom_hline(yintercept = 0,
                             linetype = "solid") + 
         ggplot2::geom_hline(yintercept = cutoff_fit_measure,
                             linetype = "dashed") + 
         ggplot2::geom_hline(yintercept = -1*cutoff_fit_measure,
                             linetype = "dashed") + 
         ggplot2::xlab("Mahalanobis Distance") + 
         ggplot2::ylab("Change in Test Statistics")
         
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
  p <- p + ggrepel::geom_label_repel(
              data = dat[(dat$md >= c_md_cut) | (dat$md >= m_md_cut), ],
              ggplot2::aes(.data$md, .data$fm, label = .data$case_id))
         
         
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
  p <- p + ggrepel::geom_label_repel(
              data = dat[(dat$gcd >= c_gcd_cut) | (dat$gcd >= m_gcd_cut), ],
              ggplot2::aes(.data$md, .data$fm, label = .data$case_id))

  p       
}

