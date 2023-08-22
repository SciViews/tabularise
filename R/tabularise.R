# TODO: continuous_summary + proc_freq + shift_table
# TODO: tabulator + tabulator_colnames + summarizor + labelizor
# TODO: also see in janitor package (tabyl(), ...)
# For the moment...
# This combines flextable(), as_flextable() (also regulartable() and
# qflextable()), as_flextable(as_grouped_data()), continuous_summary()
# Note: proc_freq() is used in as_flextable(table())

#' Tabularise an object (arrange or enter in tabular form)
#'
#' @param data An object
#' @param ... Further arguments (depending on the object class and on `type=`).
#' @param type The type of table to produce.
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the {flextable} functions from set Stb$verb().
#' @export
#' @seealso [tabularise_default()], [tabularise_headtail()], [tabularise_coef()],
#'   [tabularise_tidy()], [tabularise_glance()]
#'
#' @examples
#' tabularise(iris)
tabularise <- structure(
  function(data, ..., type = "default", env = parent.frame()) {
    get_type("tabularise", type = type)(data, ...)
  }, class = c("function", "subsettable_type", "tabularise"))

# Synonyms
#' @export
#' @rdname tabularise
tabularize <- tabularise
#tab6e <- tabularise # Alphanumeric acronym ou numeronym
