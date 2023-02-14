#' @title Plots of Case Influence on
#' Parameter Estimates
#'
#' @description Gets the output of
#' functions such as [est_change()] and
#' [est_change_approx()] and plots case
#' influence on selected parameters.
#'
#' @details The output of
#' [est_change()], [est_change_raw()],
#' [est_change_approx()], and
#' [est_change_raw_approx()] is simply a
#' matrix. Therefore, these functions
#' will work for any matrix provided.
#' Row number will be used on the x-axis
#' if applicable. However, case
#' identification values will be used
#' for labeling individual cases if they
#' are stored as row names.
#'
#' @param change The output from
#' [est_change()], [est_change_raw()],
#' [est_change_approx()], or
#' [est_change_raw_approx()].
#'
#' @param parameters If it is
#' a character vector, it
#' specifies the selected parameters.
#' Each parameter is named as in
#' `lavaan` syntax, e.g., `x ~ y` or `x
#' ~~ y`, as appeared in the columns
#' `lhs`, `op`, and `rhs` in the output
#' of [lavaan::parameterEstimates()].
#' Supports specifying an operator to
#' select all parameters with this
#' operators: `~`, `~~`, `=~`, and `~1`.
#' This vector can contain both
#' parameter names and operators.
#' If it is a numeric vector, it
#' specifies the columns to be used.
#' If omitted or `NULL`,
#' the default, changes on all
#' parameters in `change`. will be used.
#'
#' @param cutoff_change Cases with
#' absolute changes larger than this
#' value will be labeled. Default is
#' `NULL`. If `NULL`, no cutoff line
#' will be drawn.
#'
#' @param largest_change  The number of
#' cases with the largest absolute
#' changes to be labelled. Default is
#' 1. If not an integer, it will be
#' rounded to the nearest integer.
#'
#' @param cutoff_gcd Cases with
#' generalized Cook's distance or
#' approximate generalized Cook's
#' distance larger than this value will
#' be labeled. Default is `NULL`. If
#' `NULL`, no cutoff line will be drawn.
#'
#' @param largest_gcd The number of
#' cases with the largest generalized
#' Cook's distance or approximate
#' generalized Cook's distance to be
#' labelled. Default is 1. If not an
#' integer, it will be rounded to the
#' nearest integer.
#'
#' @param circle_size The size of the
#' largest circle when the size of a
#' circle is controlled by a statistic.
#'
#' @return A [ggplot2] plot. Plotted by
#' default. If assigned to a variable or
#' called inside a function, it will not
#' be plotted. Use [plot()] to plot it.
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>.
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
#' # Compute approximate case influence on parameters estimates
#' out <- est_change_approx(fit)
#'
#' # Plot case influence against approximated gCD for all
#' # regression coefficients
#' # Label top 5 cases with largest approximated gCD
#' est_change_gcd_plot(out,
#'                     parameters = "~",
#'                     largest_gcd = 5)
#'
#'
#' @references Pek, J., & MacCallum, R.
#'  (2011). Sensitivity analysis in
#'  structural equation models: Cases
#'  and their influence. *Multivariate
#'  Behavioral Research, 46*(2),
#'  202-228.
#'  doi:10.1080/00273171.2011.561068
#'
#' @seealso [est_change()], [est_change_raw()],
#' [est_change_approx()], and [est_change_raw_approx()].
#'
#' @name est_change_plot
NULL

#' @importFrom rlang .data
#' @describeIn est_change_plot Index plot of case influence on parameters.
#' @export

est_change_plot <- function(change,
                            parameters,
                            cutoff_change = NULL,
                            largest_change = 1) {
    parameters <- params_selected(change = change,
                                  parameters = parameters)
    x1 <- cbind(change, gcd = NA)
    x0 <- est_to_long(x1)
    if (!missing(parameters)) {
        x0 <- x0[x0$param %in% parameters, ]
      }
    x0$gcd <- NULL
    x0$row_id <- as.integer(seq_len(nrow(change)))
    p <- ggplot2::ggplot(data = x0, ggplot2::aes(x = .data[["row_id"]],
                                                 y = .data[["change"]])) +
          ggplot2::geom_point() +
          # ggplot2::labs(title = gcd_label) +
          ggplot2::geom_segment(
                      ggplot2::aes(xend = .data[["row_id"]],
                                   yend = 0),
                                   linewidth = 1,
                                   lineend = "butt") +
          ggplot2::geom_hline(yintercept = 0,
                              linetype = "solid",
                              color = "grey") +
          ggplot2::xlab("Row ID") +
          ggplot2::ylab("Change")
    if (is.numeric(cutoff_change)) {
        p <- p + ggplot2::geom_hline(yintercept = cutoff_change,
                                    linetype = "dashed")
        p <- p + ggplot2::geom_hline(yintercept = -1 * cutoff_change,
                                    linetype = "dashed")
        c_change_i <- abs(x0$change) >= cutoff_change
      } else {
        c_change_i <- rep(FALSE, nrow(x0))
      }
    if (is.numeric(largest_change) && largest_change >= 1) {
        o_change <- tapply(abs(x0$change),
                           INDEX = x0$param,
                           FUN = order,
                           decreasing = TRUE,
                           simplify = FALSE)
        tmp <- lapply(o_change, function(x) {
                          out <- rep(FALSE, length(x))
                          out[x[seq_len(largest_change)]] <- TRUE
                          out
                        })
        m_change_i <- unlist(tmp[unique(x0$param)])
      } else {
        m_change_i <- rep(FALSE, nrow(x0))
      }
    label_i <- c_change_i | m_change_i
    p <- p + ggrepel::geom_label_repel(
                data = x0[label_i, ],
                ggplot2::aes(x = .data[["row_id"]],
                             y = .data[["change"]],
                             label = .data[["case"]]),
                # position = ggplot2::position_dodge(.25),
                min.segment.length = 0)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["param"]]),
                        ncol = 1,
                        scales = "free_y",
                        strip.position = "left")
    p
  }

#' @importFrom rlang .data
#' @describeIn est_change_plot Plot case influence against
#' generalized Cook's distance.
#' @export

est_change_gcd_plot <- function(change,
                                parameters,
                                cutoff_gcd = NULL,
                                largest_gcd = 1,
                                circle_size = 10) {
    parameters <- params_selected(change = change,
                                  parameters = parameters)
    gcd_name <- gcd_method(change)
    gcd_name_short <- switch(gcd_name,
                             gcd = "gCD",
                             gcd_approx = "gCD Approx.")
    approx <- ifelse(gcd_name == "gcd_approx", TRUE, FALSE)
    x0 <- est_to_long(change)
    if (!missing(parameters)) {
        x0 <- x0[x0$param %in% parameters, ]
      }
    p <- ggplot2::ggplot(data = x0,
                ggplot2::aes(x = .data[[gcd_name]],
                             y = .data[["change"]])) +
          ggplot2::geom_point(ggplot2::aes(size = .data[[gcd_name]]),
                              shape = 1) +
          ggplot2::geom_hline(yintercept = 0)
    if (is.numeric(cutoff_gcd)) {
        p <- p + ggplot2::geom_vline(xintercept = cutoff_gcd,
                                     linetype = "dashed")
        c_gcd_cut <- cutoff_gcd
      } else {
        c_gcd_cut <- Inf
      }
    if (is.numeric(largest_gcd) && largest_gcd >= 1) {
        gcd_tmp <- x0[x0$param == x0$param[1], c("case",
                                                 gcd_name)]
        m_gcd <- round(largest_gcd)
        o_gcd <- order(gcd_tmp[[gcd_name]],
                               decreasing = TRUE)
        m_gcd_cut <- gcd_tmp[[gcd_name]][o_gcd[m_gcd]]
      } else {
        m_gcd_cut <- Inf
      }
    label_gcd <- (x0[[gcd_name]] >= c_gcd_cut) |
                 (x0[[gcd_name]] >= m_gcd_cut)
    p <- p + ggrepel::geom_label_repel(
                data = x0[label_gcd, ],
                ggplot2::aes(x = .data[[gcd_name]],
                             y = .data[["change"]],
                             label = .data[["case"]]),
                # position = ggplot2::position_dodge(.25),
                min.segment.length = 0)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["param"]]),
                        ncol = 1,
                        scales = "free_x",
                        strip.position = "left")
    p <- p + ggplot2::scale_size_area(name = gcd_name_short,
                                      max_size = circle_size)
    p <- p + ggplot2::xlab(switch(gcd_name,
              gcd = "Generalized Cook's Distance",
              gcd_approx = "Generalized Cook's Distance (Approximated)"))
    p
  }

#' @title Find the Method Used to
#' Compute gCD
#' @noRd

gcd_method <- function(x,
                       gcd_names = c("gcd", "gcd_approx")) {
    gcd_name <- intersect(gcd_names, colnames(x))
    if (length(gcd_name) != 1) {
        stop("The method used cannot be determined")
      }
    gcd_name
  }

#' @title Convert the Output of Case
#' Influence on Parameter Estimates to
#' Long Format
#' @noRd

est_to_long <- function(x) {
    gcd_name <- gcd_method(x)
    tmp <- which(colnames(x) == gcd_name)
    x0 <- x[, -tmp]
    out <- data.frame(change = as.vector(x0))
    n <- nrow(x0)
    k <- ncol(x0)
    out$param <- rep(colnames(x0), each = n)
    out$case <- rep(rownames(x0), times = k)
    out[, gcd_name] <- rep(x[, gcd_name], times = k)
    attr(out, "gcd_name") <- gcd_name
    out
  }

#' @title Get the Columns Requested
#' @noRd

params_selected <- function(change, parameters) {
    pnames <- gsub(" ", "", colnames(change), fixed = TRUE)
    pnames <- setdiff(pnames, c("gcd", "gcd_approx"))
    if (missing(parameters)) {
        return(pnames)
      }
    parameters0 <- gsub(" ", "", parameters)
    p_cov <- pnames[grepl("~~", pnames)]
    tmp <- setdiff(pnames, p_cov)
    p_int <- tmp[grepl("~1", tmp)]
    tmp <- setdiff(tmp, p_int)
    p_lambda <- tmp[grepl("=~", tmp)]
    tmp <- setdiff(tmp, p_lambda)
    p_def <- tmp[grepl(":=", tmp)]
    tmp <- setdiff(tmp, p_def)
    p_beta <- tmp[grepl("~", tmp)]
    out <- character(0)
    if ("~" %in% parameters0) {
        out <- c(out, p_beta)
      }
    if (":=" %in% parameters0) {
        out <- c(out, p_def)
      }
    if ("=~" %in% parameters0) {
        out <- c(out, p_lambda)
      }
    if ("~1" %in% parameters0) {
        out <- c(out, p_int)
      }
    if ("~~" %in% parameters0) {
        out <- c(out, p_cov)
      }
    out <- c(out, intersect(pnames, parameters))
    out <- pnames[match(out, pnames)]
    if (length(out) == 0) {
        stop("All the selected parameters not found.")
      }
    out
  }