#' Create Tabular Outputs from R (using "Flextable")
#'
#' Create rich-formatted tabular outputs from various R objects that can be
#' incorporated into R Markdown/Quarto documents with correct output at least in
#' HTML, LaTeX/PDF, Word and PowerPoint for various R objects.
#'
#' For an object class, there is a "default" method that creates the most
#' obvious tabular form for this object, but there might be also other types
#' provided with different tabular views of the same object. All types are
#' accessible from the [tabularise()] function that accepts `type=` argument,
#' or better, by using the compact and easier to read `tabularise$type()` form.
#'
#' @section Important functions:
#'
#'- [tabularise()] constructs a **flextable** object.
#'
#'- [Stb] a collection of functions to manipulate and format a table (mostly
#'  functions from the {flextable} package.
#'
#'- [colformat_sci()] and [colformat_md()] are additional columns formatters for
#'   **flextable** objects, respectively, scientific and markdown formats.
#'
#' - [para_md()] creates {flextable} paragraphs with rich-formatting by
#'   converting Markdown strings into such {flextable} paragraphs.
#'
#' @docType package
#' @name tabularise-package

## usethis namespace: start
#' @importFrom svMisc aka section
#' @importFrom rlang %||% is_interactive f_rhs get_expr new_function pairlist2 quo
#' @importFrom utils apropos head tail
#' @importFrom stats terms coef
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname){
  # Define default themes for tables
  options(tinytable_tt_theme = theme_tt_sciviews)
  flextable::set_flextable_defaults(big.mark = "\u00a0",
    font.family = "Arial", font.size = 12)
}
