#' "Tidy" type tabularise generic (tabularise$tidy)
#'
#' The "tidy" type of [tabularise()] presents a tidy version of an object, as
#' it could be obtained by [generics::tidy()] whose goal is to turn information
#' contained in an object into a rectangular table. Here, the table is nicely
#' formatted as an (almost) publication-ready form (good for informal reports,
#' notebooks, etc).
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
#' with the {flextable} functions from set Stb$verb().
#' @export
#' @importFrom generics tidy
#' @seealso [tabularise()], [generics::tidy()]
tabularise_tidy <- function(data, ..., env = env) {
  UseMethod("tabularise_tidy")
}

#' @export
#' @rdname tabularise_tidy
#' @method tabularise_tidy default
tabularise_tidy.default <- function(data, ..., kind = "ft", env = env) {
  stop("No method for tabularise_tidy() for this object")
}
