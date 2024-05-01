#' "Confint" type" tabularise generic (tabularise$confint)
#'
#' The "confint" type of [tabularise()] presents a table of confidence intervals
#' for an objects (e.g., confidence intervals on parameters of a model). This is
#' similar to the output of [stats::confint()] generic function on the same
#' object. The nicely formatted table obtained here is (almost)
#' publication-ready (good for informal reports, notebooks, etc).
#'
#' #' No useful method for this type is defined in the \{tabularise\} package, but
#' additional packages might define some.
#'
#' @param data An object
#' @param ... Further arguments (depending on the object class).
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the \{flextable\} functions from set Stb$verb().
#' @export
#' @importFrom stats confint
#' @seealso [tabularise()], [stats::confint()]
tabularise_confint <- function(data, ..., kind = "ft", env = env) {
  UseMethod("tabularise_confint")
}

#' @export
#' @rdname tabularise_confint
#' @method tabularise_confint default
tabularise_confint.default <- function(data, ..., kind = "ft", env = env) {
  stop("No method for tabularise_confint() for this object")
}
