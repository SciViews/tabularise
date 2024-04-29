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
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the \{flextable\} functions from set Stb$verb().
#' @export
#' @importFrom flextable set_caption
#' @importFrom knitr opts_current
#' @seealso [tabularise_default()], [tabularise_headtail()], [tabularise_coef()],
#'   [tabularise_tidy()], [tabularise_glance()]
#'
#' @examples
#' tabularise(iris)
tabularise <- structure(
  function(data, ..., type = "default", kind = "ft", env = parent.frame()) {
    tb <- get_type("tabularise", type = type)(data, ..., kind = kind, env = env)
    # Solve different problems related to table captions in R Markdown or Quarto
    opts_current <- knitr::opts_current
    lbl <- opts_current$get('label')
    cap <- opts_current$get('tbl-cap')

    # If no caption is defined in the chunk that generates this table,
    # we fake one if the label exists and does not start with `tbl-`
    # since it means we do not want a caption (and a cross reference)
    # to this table anyway. Otherwise, Quarto produces an error.
    if (!is.null(lbl) && substring(lbl, 1L, 4L) != "tbl-" && is.null(cap)) {
      # We fake one, so Quarto does not complains any more for missing caption
      opts_current$set(`tbl-cap` = "")
      # Note: caption does not change outside the chunk, that is, it is not
      # modified outside this code and the table has NO caption, but at least
      # no error is generated in Quarto
    }

    # Make sure caption is correct both with R Markdown and quarto, both using
    # tbl-cap chunk option
    if (!is.null(cap))
      tb <- set_caption(tb, cap) # This is for R Markdown documents
    tb
  }, class = c("function", "subsettable_type", "tabularise"))

# Synonyms
#' @export
#' @rdname tabularise
tabularize <- tabularise
#tab6e <- tabularise # Alphanumeric acronym ou numeronym
