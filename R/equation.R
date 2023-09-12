#' Get a LaTeX equation from a model
#'
#' @description
#' Extract or create a LaTeX equation to describe a model. All objects supported
#' by [equatiomatic::extract_eq()] are supported by the default method
#'  description.
#' @param object An object with a model whose the equation is constructed. If a
#'   **character** object is provided, it is concatenated into a single
#'   character string and the **equation** class, otherwise non transformed (it
#'   is supposed to be a valid LaTeX equation). Remember that backslashes must be
#'   doubled in regular R strings, or use the `r"(...)"` notation, which may be
#'   more comfortable here, see examples.
#' @param ... Further parameters passed to [equatiomatic::extract_eq()] (see its
#'   man page).
#'
#' @return An object of class `c("equation", "character")`.
#' @export
#' @importFrom equatiomatic extract_eq
#'
#' @details
#' There are slight differences between `equation()`, `eq_()` and `eq__()`:
#' * `equation()` returns a string with LaTeX code and prints LaTeX code at the
#'    R console.
#' * `eq_()` returns the LaTeX code surrounded by a dollar sign `$...$` and is
#'    suitable to build inline equations in R Markdown/Quarto documents by using
#'    inline R code. It prints the rendered inline equation in the RStudio
#'    Viewer or in the browser
#' * `eq__()` returns the LaTeX code _not_ surrounded by dollar signs, and it
#'    also prints a rendered version in the Viewer pane of RStudio or in the
#'    browser. It should be used in an R inline chunk inside a `$$...$$`
#'    construct in a Markdown text. The result is a display equation that can
#'    also be cross referenced in Quarto in the usual way if you label it, e.g.,
#'    you use `$$...$$ {#eq-label}`.
#'
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length + Species)
#' summary(iris_lm)
#' equation(iris_lm)
#' # Providing directly the LaTeX code of the equation (variance of a sample)
#' equation("S^2_y = \\sum_{i=1}^{n-1} \\frac{(y_i - \\bar{Y})^2}{n}")
#' # Easier to write like this (avoiding the double backslashes):
#' eq1 <- equation(r"(S^2_y = \sum_{i=1}^{n-1} \frac{(y_i - \bar{Y})^2}{n})")
#' # Print raw text:
#' eq1
#' # Get a preview of the equation
#' eq__(eq1)
#' # The same function can be used inside a `$$...$$ {#eq-label}` construct in
#' # R Markdown or Quarto to calcule a display equation that is also recognized
#' # by the cross referencing system of Quarto.
#' # Get a string suitable for inclusion inline in R Markdown with `r eq_(eq1)`
#' # (inside Markdown text and without the dollar signs around it)
#' eq_(eq1)
equation <- function(object, ...) {
  UseMethod("equation")
}

#' @rdname equation
#' @export
#' @method equation default
equation.default <- function(object, ...) {
  equatiomatic::extract_eq(object, ...)
}

#' @rdname equation
#' @export
#' @method equation character
equation.character <- function(object, ...) {
  eq <- paste(object, collapse = "\n")
  class(eq) <- c("equation", "character")
  eq
}

#' @rdname equation
#' @export
eq__ <- function(object, ...) {
  eq <- equation(object, ...)
  class(eq) <- c("formatted_equation", "equation", "character")
  eq
}

#' @rdname equation
#' @export
eq_ <- function(object, ...) {
  eq <- paste0("$", paste(equation(object, collapse = "\n"), ...), "$")
  class(eq) <- c("inline_equation", "character")
  eq
}

#' @rdname equation
#' @export
#' @param x A **formatted_equation** or **inline_equation** object.
#' @method print formatted_equation
print.formatted_equation <- function(x, ...) {
  # TODO: use double $$s, once display equation will be supported by para_md()
  print(para_md(paste0("$", paste(x, collapse = "\n"), "$")))
  invisible(x)
}

#' @rdname equation
#' @export
#' @method print inline_equation
print.inline_equation <- function(x, ...) {
  print(para_md(x))
  invisible(x)
}
