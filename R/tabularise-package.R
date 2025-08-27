#' Create Tabular Outputs from R
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
#'- [tabularise()] constructs a table (**flextable** object by default, or
#'   **tinytable** object with `kind = "tt"`).
#'
#'- [Stb] a collection of functions to manipulate and format a table (mostly
#'  functions from the \{flextable\} package.
#'
#'- [colformat_sci()] and [colformat_md()] are additional columns formatters for
#'   **flextable** objects, respectively, scientific and markdown formats.
#'
#' - [para_md()] creates \{flextable\} paragraphs with rich-formatting by
#'   converting Markdown strings into such \{flextable\} paragraphs.
#'
## usethis namespace: start
#' @importFrom svBase aka args_type get_type list_types name_function_type section
#' @importFrom rlang %||% is_interactive f_rhs get_expr new_function pairlist2 quo
#' @importFrom utils apropos head tail
#' @importFrom stats terms coef
#' @importFrom flextable as_equation as_paragraph
#' @importFrom equatiomatic equation eq_ eq__ extract_eq
#' @importFrom equatags transform_mathjax
## usethis namespace: end
"_PACKAGE"
