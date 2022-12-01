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
#' "partable" (parameter table) or "coef"
#' (coefficient vector).
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
                              "partable")) {
    where <- match.arg(where)
    pfree <- lavaan::lavInspect(fit, "npar")
    ngp <- lavaan::lavInspect(fit, "ngroups")
    ptable <- lavaan::parameterTable(fit)
    ptable$rowid <- seq_len(nrow(ptable))
    parspt <- tryCatch(lavaan::lavaanify(pars, ngroups = ngp),
                       error = function(e) e)
    if (inherits(parspt, "simpleError")) {
        return(numeric(0))
        # stop(paste0("Error in parameter syntax. This is the lavaan error message:",
        #             "\n",
        #             parspt$message))
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

#' @title Get id based on lhs-op-rhs and group
#'
#' @noRd

pars_id_lorg <- function(pars,
                        fit,
                        where = c("coef",
                                  "partable")) {
    where <- match.arg(where)
    pfree <- lavaan::lavInspect(fit, "npar")
    ngp <- lavaan::lavInspect(fit, "ngroups")
    glabels <- lavaan::lavInspect(fit, "group.label")
    ptable <- lavaan::parameterTable(fit)
    # Do not use user-supplied labels
    ptable$label <- ""
    ptable$rowid <- seq_len(nrow(ptable))
    ptable$lavlabel <- lavaan::lav_partable_labels(ptable, type = "user")
    pars_c <- sapply(pars, function(x) {
                              gsub(x = x,
                                   pattern = " ",
                                   replacement = "",
                                   fixed = TRUE)
                            }, USE.NAMES = FALSE)
    if (ngp > 1) {
        lavlabel_gp <- get_g1(ptable$lavlabel)
        ptable$lavlabel <- add_g1(ptable$lavlabel)
        # pars w gp label (gX)
        for (x in seq_along(glabels)) {
            pars_c <- gsub(paste0("\\.", glabels[x], "$"), paste0(".g", x), pars_c)
          }
        x <- pars_c %in% lavlabel_gp
        pars_c2 <- as.vector(sapply(pars_c[x],
                                function(y) paste0(y, ".g", seq_len(ngp))))
        pars_c <- c(pars_c[!x], pars_c2)
      } else {
        lavlabel_gp <- ptable$lavlabel
        x <- pars_c %in% lavlabel_gp
        pars_c <- pars_c[x]
      }
    parspt4 <- ptable[ptable$lavlabel %in% pars_c, ]
    if (where == "partable") {
        out <- parspt4$rowid
      }
    if (where == "coef") {
        out <- parspt4$free
      }
    out
  }

#' @title Find First Group Parameters
#' @noRd

fix_g1 <- function(x, action = c("get", "add")) {
    action <- match.arg(action)
    lavlabels <- x
    a1 <- grepl("^.*\\.g2$", lavlabels)
    lavlabels_ng <- gsub(".g2", "", lavlabels[a1])
    a2 <- lavlabels %in% lavlabels_ng
    if (action == "get") {
        return(lavlabels[a2])
      }
    if (action == "add") {
        lavlabels[a2] <- paste0(lavlabels[a2], ".g1")
        return(lavlabels)
      }
    stop("Unknown action in fix_g1().")
  }

add_g1 <- function(x) {
    fix_g1(x, action = "add")
  }

get_g1 <- function(x) {
    fix_g1(x, action = "get")
  }


#' @title Get id based on operator
#' @noRd

pars_id_op <- function(pars,
                       fit,
                       where = c("coef",
                                 "partable")) {

  }

#' @title Get id based on wildcard
#' @noRd

pars_id_wild <- function(pars,
                         fits,
                         where = c("coef",
                                   "partable")) {

  }

#' @title Get id for special parameters
#' @noRd

pars_id_special <- function(pars,
                            fit,
                            where = c("coef",
                                      "partable")) {

  }

