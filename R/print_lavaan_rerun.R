#' @title Print the results of lavaan_rerun
#'
#' @description Print the results of lavaan_rerun
#'
#' @details Print the results of lavaan_rerun
#'
#' @return
#'  Nothing
#'
#' @param x The output of [lavaan_rerun()].
#' @param ... Other arguments. They will be ignored.
#'
#' @examples
#' # TODO
#'
#' @export

setGeneric("print")

print.lavaan_rerun <- function(x,
                          ...) {
    nrerun <- length(x$rerun)
    n_checked <- sum(sapply(x$post_check, isTRUE))
    n_failed <- sum(sapply(x$post_check, isFALSE))
    org_call <- x$call
    cat("=== lavaan_rerun Output ===\n")
    cat("Call:\n")
    print(org_call)
    cat(paste0("Number of reruns: ", nrerun, "\n"))
    cat(paste0("Number of reruns that passed post.check of lavaan: ",
                n_checked, "\n"))
    cat(paste0("Number of reruns that failed post.check of lavaan: ",
                n_failed, "\n"))
  }
