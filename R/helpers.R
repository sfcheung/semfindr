est_names_by_op <- function(est, op) {
    est1 <- est[est$op %in% op, ]
    out <- paste0(est1$lhs, est1$op, est1$rhs)
    out
  }

est_names_selected <- function(est, params) {
    pnames_full <- paste0(est$lhs, est$op, est$rhs)
    pnames_user <- gsub(" ", "", params)
    out1 <- pnames_full[which(pnames_full %in% pnames_user)]
    out2 <- est_names_by_op(est, params)
    out <- unique(c(out1, out2))
    out
  }