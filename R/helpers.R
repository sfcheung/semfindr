#' @noRd

est_names_by_op <- function(est, op) {
    est1 <- est[est$op %in% op, ]
    out <- paste0(est1$lhs, est1$op, est1$rhs)
    out
  }

#' @noRd

est_names_selected <- function(est, params) {
    pnames_full <- paste0(est$lhs, est$op, est$rhs)
    pnames_user <- gsub(" ", "", params)
    out1 <- pnames_full[which(pnames_full %in% pnames_user)]
    out2 <- est_names_by_op(est, params)
    out <- unique(c(out1, out2))
    out
  }

#' @noRd

est_names_free <- function(fit) {
    ptable <- lavaan::parameterTable(fit)
    # For using lhs-op-rhs
    ptable$label <- ""
    lavaan::lav_partable_labels(ptable, type = "free")
  }

#' @noRd

est_ids_selected <- function(fit, params) {
    # This function returns all valid rows, free or not
    ops <- c("~~", "~", "=~", "~1", "|", "~*~", "<~")
    pars1 <- setdiff(params, ops)
    if (length(pars1) > 0) {
        out1 <- pars_id(pars = params, fit = fit, where = "partable")
      } else {
        out1 <- numeric(0)
      }
    pars2 <- intersect(params, ops)
    if (length(pars2) > 0) {
        ptable <- lavaan::parameterTable(fit)
        out2 <- ptable$id[ptable$op %in% pars2]
      } else {
        out2 <- numeric(0)
      }
    out <- union(out1, out2)
    out
  }

#' @noRd

lav_data_used <- function(fit) {
    data_full <- lavaan::lavInspect(fit, "data")
    ngroups <- lavaan::lavInspect(fit, "ngroups")
    if (ngroups > 1) {
        gp_labels <- lavaan::lavInspect(fit, "group.label")
        gp_var <- lavaan::lavInspect(fit, "group")
        tmpfct <- function(x, gp_label, gp_var) {
            out <- as.data.frame(x)
            out[gp_var] <- gp_label
            out
          }
        data_full <- mapply(tmpfct,
                            x = data_full,
                            gp_label = gp_labels,
                            MoreArgs = list(gp_var = gp_var),
                            SIMPLIFY = FALSE,
                            USE.NAMES = TRUE)
        data_full <- do.call(rbind, data_full)
        rownames(data_full) <- NULL
        ii <- unlist(lavaan::lavInspect(fit, "case.idx"))
        data_full <- data_full[order(ii), ]
        rownames(data_full) <- NULL
      }
    return(data_full)
  }

full_rank <- function(x) {
    # Idea based on WeightIt::make_full_rank
    x <- check_square(x)
    if (ncol(x) == 1) {
        out <- list(final = x,
                    original = x,
                    dropped = integer(0))
        return(out)
      }
    tmp <- x
    rownames(tmp) <- seq_len(nrow(tmp))
    colnames(tmp) <- seq_len(ncol(tmp))
    x_rank <- rankMatrix_square(x)
    dropped <- integer(0)
    while (x_rank != ncol(tmp)) {
        for (i in seq_len(ncol(tmp))) {
            tmp2 <- tmp[-i, -i, drop = FALSE]
            tmp2_rank <- rankMatrix_square(tmp2)
            if (tmp2_rank == x_rank) {
                break
              }
          }
        dropped <- c(dropped, as.integer(colnames(tmp)[i]))
        tmp <- tmp[-i, -i, drop = FALSE]
        x_rank <- rankMatrix_square(tmp)
      }
    out <- list(final = tmp,
                original = x,
                dropped = dropped)
    return(out)
  }

rankMatrix_square <- function(x) {
    x <- check_square(x)
    if (ncol(x) == 1) {
        return(1)
      } else {
        return(Matrix::rankMatrix(x))
      }
  }

#' @noRd

check_square <- function(x) {
    if (length(dim(x)) != 2) {
        stop("x is not a 2-dimensional matrix.")
      }
    if (ncol(x) != nrow(x)) {
        stop("x is not a square matrix.")
      }
    x
  }

#' @noRd

to_est_change_from_influence_stat <- function(x) {
    if (!inherits(x, "influence_stat")) {
        stop("The object is of an influence_stat object.")
      }
    pnames <- attr(x, "parameters_names")
    if (is.null(pnames)) {
        warnings("Case influence on parameters not available.")
      }
    xx <- x[, pnames, drop = FALSE]
    attributes(xx) <- attr(x, "parameters_attrs")
    class(xx) <- c("est_change", class(xx))
    invisible(xx)
  }
