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
#' The default settings for the plots
#' should be good enough for diagnostic
#' purpose. If so desired, users can
#' use the `*_aes` arguments to nearly
#' fully customize all the major
#' elements of the plots, as they would
#' do for building a `ggplot2` plot.
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
#' @param title If `TRUE`, the default,
#' a default title will be added to
#' the plot. If it is a string, it will
#' be used as the title. If `FALSE`,
#' no title will be added to the plot.
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
#' in the index plot by
#' [est_change_plot()]. Default is
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
#' [ggplot2::geom_hline()] in
#' [est_change_plot()] or
#' [ggplot2::geom_vline()] in
#' [est_change_gcd_plot()] to modify how
#' to draw the line for user cutoff
#' value (`cutoff_change` or
#' `cutoff_gcd`). Default is `list()`
#' and internal default settings will be
#' used.
#'
#' @param case_label_aes A named list of
#' arguments to be passed to
#' [ggrepel::geom_label_repel()] to
#' modify how to draw the labels for
#' cases marked (based on
#' `cutoff_change`, `cutoff_gcd`,
#' `largest_change`, or `largest_gcd`).
#' Default is `list()` and internal
#' default settings will be used.
#'
#' @param wrap_aes A named list of
#' arguments to be passed to
#' [ggplot2::facet_wrap()] to modify how
#' the plots are organized. Default is
#' `list()` and internal default
#' settings will be used.
#'
#' @return A `ggplot2` plot. Plotted by
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
#' # Plot case influence for all regression coefficients
#' est_change_plot(out,
#'                 parameters = "~",
#'                 largest_change = 2)
#'
#' # Plot case influence against approximated gCD for all
#' # regression coefficients
#' # Label top 5 cases with largest approximated gCD
#' est_change_gcd_plot(out,
#'                     parameters = "~",
#'                     largest_gcd = 5)
#'
#' # Customize elements in a plot.
#' # For example, change the color and shape of the points.
#'
#' est_change_plot(out,
#'                 parameters = "~",
#'                 largest_change = 2,
#'                 point_aes = list(shape = 5,
#'                                  color = "red"))
#'
#' @references Pek, J., & MacCallum, R.
#'  (2011). Sensitivity analysis in
#'  structural equation models: Cases
#'  and their influence. *Multivariate
#'  Behavioral Research, 46*(2),
#'  202-228.
#'  doi:10.1080/00273171.2011.561068
#'
#' @seealso [est_change()],
#' [est_change_raw()],
#' [est_change_approx()], and
#' [est_change_raw_approx()].
#'
#' @name est_change_plot
NULL

#' @importFrom rlang .data
#' @describeIn est_change_plot Index
#' plot of case influence on parameters.
#' @export

est_change_plot <- function(change,
                            parameters,
                            cutoff_change = NULL,
                            largest_change = 1,
                            title = TRUE,
                            point_aes = list(),
                            vline_aes = list(),
                            hline_aes = list(),
                            cutoff_line_aes = list(),
                            case_label_aes = list(),
                            wrap_aes = list()
                            ) {

    if (inherits(change, "influence_stat")) {
        change <- to_est_change_from_influence_stat(change)
      }

    point_aes <- utils::modifyList(list(shape = 21,
                                        color = "black",
                                        fill = "grey",
                                        alpha = .75,
                                        size = 1),
                                   point_aes)

    vline_aes <- utils::modifyList(list(linewidth = .5,
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

    parameters <- params_selected(change = change,
                                  parameters = parameters)
    # x1 <- cbind(change, gcd = NA)
    x0 <- est_to_long(change)
    if (!missing(parameters)) {
        x0 <- x0[x0$param %in% parameters, ]
      }
    x0$gcd <- NULL
    x0$row_id <- as.integer(seq_len(nrow(change)))
    x0 <- stats::na.omit(x0)
    if (nrow(x0) == 0) {
        stop("No cases have valid values.")
      }
    p <- ggplot2::ggplot(data = x0, ggplot2::aes(x = .data[["row_id"]],
                                                 y = .data[["change"]]))
    p <- p + do.call(ggplot2::geom_point, point_aes)
    p <- p + do.call(ggplot2::geom_segment, vline_aes)
    p <- p + do.call(ggplot2::geom_hline, hline_aes)
    p <- p + ggplot2::xlab("Row ID") +
             ggplot2::ylab("Change")
    if (is.numeric(cutoff_change)) {
        cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"),
                                                  cutoff_line_aes)
        # The following part should never be changed by users.
        cutoff_line_aes1 <- utils::modifyList(cutoff_line_aes,
                                      list(yintercept = cutoff_change))
        cutoff_line_aes2 <- utils::modifyList(cutoff_line_aes,
                                      list(yintercept = -cutoff_change))
        p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes1)
        p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes2)
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

    case_label_aes <- utils::modifyList(list(min.segment.length = 0),
                                   case_label_aes)
    # The following part should never be changed by users.
    case_label_aes <- utils::modifyList(case_label_aes,
                          list(data = x0[label_i, ],
                               mapping = ggplot2::aes(
                                  x = .data[["row_id"]],
                                  y = .data[["change"]],
                                  label = .data[["case"]])))
    p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)

    wrap_aes <- utils::modifyList(list(ncol = 1,
                                       scales = "free_y",
                                       strip.position = "left"),
                                   wrap_aes)
    # The following part should never be changed by users.
    wrap_aes <- utils::modifyList(wrap_aes,
                          list(facets = ggplot2::vars(.data[["param"]])))

    p <- p + do.call(ggplot2::facet_wrap, wrap_aes)
    if (is.logical(title)) {
        if (title) {
            plot_title <- "Case Influence on Parameter Estimates"
          }
      } else if (is.character(title)) {
            plot_title <- title
      } else {
        plot_title <- character(0)
      }
    if (length(plot_title) > 0) {
        p <- p + ggplot2::labs(title = plot_title)
      }
    p
  }

#' @importFrom rlang .data
#' @describeIn est_change_plot Plot case
#' influence on parameter estimates
#' against generalized Cook's distance.
#' @export

est_change_gcd_plot <- function(change,
                                parameters,
                                cutoff_gcd = NULL,
                                largest_gcd = 1,
                                cutoff_change = NULL,
                                largest_change = 1,
                                title = TRUE,
                                point_aes = list(),
                                hline_aes = list(),
                                cutoff_line_aes = list(),
                                case_label_aes = list(),
                                wrap_aes = list()
                                ) {

    if (inherits(change, "influence_stat")) {
        change <- to_est_change_from_influence_stat(change)
      }

    point_aes <- utils::modifyList(list(shape = 21,
                                        color = "black",
                                        fill = "grey",
                                        alpha = .75,
                                        size = 1),
                                   point_aes)

    hline_aes <- utils::modifyList(list(linetype = "solid",
                                        color = "black",
                                        linewidth = .5),
                                   hline_aes)
    # The following part should never be changed by users.
    hline_aes <- utils::modifyList(hline_aes,
                                   list(yintercept = 0))

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
    x0 <- stats::na.omit(x0)
    if (nrow(x0) == 0) {
        stop("No cases have valid values.")
      }
    p <- ggplot2::ggplot(data = x0,
                ggplot2::aes(x = .data[[gcd_name]],
                             y = .data[["change"]]))
    p <- p + do.call(ggplot2::geom_point, point_aes)
    p <- p + do.call(ggplot2::geom_hline, hline_aes)

    if (is.numeric(cutoff_change)) {
        cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"),
                                                  cutoff_line_aes)
        # The following part should never be changed by users.
        cutoff_line_aes1 <- utils::modifyList(cutoff_line_aes,
                                      list(yintercept = cutoff_change))
        cutoff_line_aes2 <- utils::modifyList(cutoff_line_aes,
                                      list(yintercept = -cutoff_change))
        p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes1)
        p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes2)
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

    if (is.numeric(cutoff_gcd)) {
        cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"),
                                      cutoff_line_aes)
        # The following part should never be changed by users.
        cutoff_line_aes <- utils::modifyList(cutoff_line_aes,
                                      list(xintercept = cutoff_gcd))
        p <- p + do.call(ggplot2::geom_vline, cutoff_line_aes)
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
                 (x0[[gcd_name]] >= m_gcd_cut) |
                 label_i

    case_label_aes <- utils::modifyList(list(min.segment.length = 0),
                                   case_label_aes)
    # The following part should never be changed by users.
    case_label_aes <- utils::modifyList(case_label_aes,
                          list(data = x0[label_gcd, ],
                               mapping = ggplot2::aes(
                                  x = .data[[gcd_name]],
                                  y = .data[["change"]],
                                  label = .data[["case"]])))
    p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)

    wrap_aes <- utils::modifyList(list(ncol = 1,
                                       scales = "free_y",
                                       strip.position = "left"),
                                   wrap_aes)
    # The following part should never be changed by users.
    wrap_aes <- utils::modifyList(wrap_aes,
                          list(facets = ggplot2::vars(.data[["param"]])))
    p <- p + do.call(ggplot2::facet_wrap, wrap_aes)

    p <- p + ggplot2::xlab(switch(gcd_name,
              gcd = "Generalized Cook's Distance",
              gcd_approx = "Generalized Cook's Distance (Approximated)"))
    if (is.logical(title)) {
        if (title) {
            plot_title <- switch(gcd_name,
              gcd = "Case Influence on Parameter Estimates Against gCD",
              gcd_approx = "Case Influence on Parameter Estimates Against Approximated gCD")
          }
      } else if (is.character(title)) {
            plot_title <- title
      } else {
        plot_title <- character(0)
      }
    if (length(plot_title) > 0) {
        p <- p + ggplot2::labs(title = plot_title)
      }
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
    gcd_name <- tryCatch(gcd_method(x),
                         error = function(e) e)
    if (inherits(gcd_name, "error")) {
        x <- cbind(x, gcd = NA)
        gcd_name <- "gcd"
      }
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
    # Does not yet support selecting parameters from a sample
    # in mulitsample models.
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