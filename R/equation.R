#' Get a LaTeX equation from a model
#'
#' @description
#' Extract or create a LaTeX equation to describe a model. All objects supported
#' by [equatiomatic::extract_eq()] are supported by the default method
#'  description
#' @param object An object with a model whole the equation is constructed.
#' @param ... Further parameters passed to [equatiomatic::extract_eq()] (see its
#'   man page).
#'
#' @return An object of class `c("equation", "character")`.
#' @export
#' @importFrom equatiomatic extract_eq
#'
#' @examples
#' iris_lm <- lm(data = iris, Petal.Length ~ Sepal.Length + Species)
#' summary(iris_lm)
#' equation(iris_lm)
equation <- function(object, ...) {
  UseMethod("equation")
}

#' @rdname equation
#' @export
#' @method equation default
equation.default <- function(object, ...) {
  equatiomatic::extract_eq(object, ...)
}
