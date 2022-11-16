#' @title Convert Parameter Syntax to Ids
#' or Row Numbers in the Parameter
#' Vector or Table
#'
#' @description An internal function to convert a vector of
#' parameters specified in lavaan syntax to the ids of them
#' in the vector of free parameters or the row numbers
#' in the parameter table.
#'
#' @details It uses [lavaan::lavaanify()] to parse the
#' syntax strings. Multiple sample models is supported.
#'
#' @return
#' A numeric vector of the ids in the column "free" in the
#' parameter table of the fit object.
#'
#' @param pars A character vector of parameters specified
#' in lavaan syntax, e.g., `"y ~ x"` and `f1 =~ x3`. For
#' multisample models, if only the parameters in some groups
#' are needed, use the modifier for labeling parameters and
#' use `NA` to denote parameters to be requested. E.g.,
#' `f1 =~ c(NA, 0, NA, NA) * x2` denotes the loadings of
#' `x2` on `f1` in the first, third, and fourth groups.
#'
#' @param fit A `lavaan`-class object. This object is used
#' to determine the number of groups and the parameters
#' in the model. Only parameters in `pars` that appear in
#' this model will be considered.
#'
#' @param where Where the values are to be found. Can be
#' "partable" (parameter table), "std" (standardized
#' solution table), or "coef" (coefficient vector).
#' Default is "coef".
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#'
#' @noRd

pars_id <- function(pars,
                    fit,
                    where = c("coef",
                              "partable",
                              "std")) {
    where <- match.arg(where)
    pfree <- lavaan::lavInspect(fit, "npar")
    ngp <- lavaan::lavInspect(fit, "ngroups")
    ptable <- lavaan::parameterTable(fit)
    stable <- lavaan::standardizedSolution(fit,
                                           se = FALSE)
    ptable$rowid <- seq_len(nrow(ptable))
    stable$rowid <- seq_len(nrow(stable))
    parspt <- tryCatch(lavaan::lavaanify(pars, ngroups = ngp),
                       error = function(e) e)
    if (inherits(parspt, "simpleError")) {
        stop(paste0("Error in parameter syntax. This is the lavaan error message:",
                    "\n",
                    parspt$message))
      }
    parspt2 <- as.data.frame(lavaan::lavParseModelString(pars))
    mcol <- c("lhs", "op", "rhs", "group", "free")
    parspt3 <- merge(parspt[, mcol],
                     parspt2)[, mcol]
    parspt3 <- parspt3[parspt3$free > 0, ]
    parspt4 <- merge(parspt3[, -which(mcol == "free")], ptable)
    if (where == "partable") {
        out <- parspt4$rowid
      }
    if (where == "coef") {
        out <- parspt4$free
      }
    out
  }
