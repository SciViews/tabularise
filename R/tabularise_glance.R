#' "Glance" type" tabularise generic (tabularise$glance)
#'
#' The "glance" type of [tabularise()] usually presents a very short (mostly
#' single row) summary of an object in a rich-formatted table. The table
#' presents usually the same or very similar information to what would be
#' obtained with the [generics::glance()] generic function on the same object.
#' The nicely formatted table obtained here is (almost) publication-ready (good
#' for informal reports, notebooks, etc).
#'
#' #' No useful method for this type is defined in the {tabularise} package, but
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
#' @importFrom generics glance
#' @seealso [tabularise()], [generics::glance()]
tabularise_glance <- function(data, ..., env = env) {
  UseMethod("tabularise_glance")
}

#' @export
#' @rdname tabularise_glance
#' @method tabularise_glance default
tabularise_glance.default <- function(data, ..., kind = "ft", env = env) {
  stop("No method for tabularise_glance() for this object")
}
