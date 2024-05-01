#' "Coef" type tabularise generic (tabularise$coef)
#'
#' The "coef" type of [tabularise()] extracts and formats a table of
#' coefficients from an object, similar to [stats::coef()] applied to the same
#' object, but in a rich-formatted form.
#'
#' No useful method for this type is defined in the \{tabularise\} package, but
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
#' @importFrom stats coef
#' @seealso [tabularise()], [stats::coef()]
tabularise_coef <- function(data, ..., kind = "ft", env = env) {
  UseMethod("tabularise_coef")
}

#' @export
#' @rdname tabularise_coef
#' @method tabularise_coef default
tabularise_coef.default <- function(data, ..., kind = "ft", env = env) {
  stop("No method for tabularise_coef() for this object")
}
