# Internal function for tabularise

# Copy of flextable:::get_j_from_formula() from v 0.9.2
.get_j_from_formula <- function(f, data) {
  if (length(f) > 2)
    stop("formula selection is not as expected ( ~ variables )", call. = FALSE)
  j <- attr(terms(f, data = data), "term.labels")
  j <- gsub("(^`|`$)", "", j)
  names_ <- names(data)
  invalid_names <- (!j %in% names_)
  if (any(invalid_names)) {
    invalid_names <- paste0("`", j[invalid_names], "`", collapse = ", ")
    stop(sprintf("`%s` is using unknown variable(s): %s",
      format(f), invalid_names), call. = FALSE)
  }
  j
}

# Copy of flextable:::get_columns_id() from v 0.9.2
.get_columns_id <- function(x, j = NULL) {
  maxcol <- length(x$col_keys)
  if (is.null(j)) {
    j <- seq_along(x$col_keys)
  }
  if (inherits(j, "formula")) {
    tmp_dat <- as.list(x$col_keys)
    names(tmp_dat) <- x$col_keys
    tmp_dat <- as.data.frame(tmp_dat, check.names = FALSE)
    j <- .get_j_from_formula(j, tmp_dat)
  }
  if (is.numeric(j)) {
    if (length(j) > 0 && all(j < 0)) {
      j <- setdiff(seq_along(x$col_keys), -j)
    }
    if (any(j < 1 | j > maxcol)) {
      stop(sprintf(
        "invalid columns selection\navailable range: [%s]\nissues: %s",
        paste0(range(seq_len(maxcol)), collapse = ", "),
        paste0(setdiff(j, seq_len(maxcol)), collapse = ", ")))
    }
  } else if (is.logical(j)) {
    if (length(j) != maxcol) {
      stop(sprintf(
        "invalid columns selection\n`j` should have a length of %.0f.",
        maxcol))
    } else {
      j <- which(j)
    }
  }
  else if (is.character(j)) {
    j <- gsub("(^`|`$)", "", j)
    if (any(is.na(j))) {
      stop("invalid columns selection: NA in selection")
    }
    else if (!all(is.element(j, x$col_keys))) {
      stop(sprintf("`%s` is using unknown variable(s): %s",
        "i", paste0("`", j[!is.element(j, x$col_keys)],
          "`", collapse = ",")))
    } else {
      j <- match(j, x$col_keys)
    }
  } else {
    stop("invalid columns selection: unknown selection type")
  }
  j
}

# Copy of flextable:::.filter_col_keys() from v 0.9.2
.filter_col_keys <- function(x, j, fun) {
  j <- .get_columns_id(x[["body"]], j)
  col_keys <- x$col_keys[j]
  col_keys[vapply(x[["body"]]$dataset[col_keys], fun, FUN.VALUE = NA)]
}

# Copy of flextable:::docall_display()
.docall_display <- function(col_keys, fun, x, i = NULL, part = "body") {
  if (inherits(i, "formula") && part %in% c("header", "footer"))
    stop("formulas are not supported in the 'header' and 'footer' parts.")

  for (varname in col_keys) {
    x <- flextable::mk_par(x = x, j = varname, i = i,
      value = fun(get(varname)), part = part)
  }
  x
}

# Links in R terminals and RStudio console are still experimental. So, we keep
# this function internal so that adaptation does not break the package for user
# .peek_help_link <- function(topic) {
#   if (is_interactive() && cli::ansi_has_hyperlink_support() &&
#       isTRUE(cli::ansi_hyperlink_types()$help)) { # RStudio help link
#     # topic must be package::helpage
#     pkg_page <- strsplit(topic, "::", fixed = TRUE)[[1]]
#     # Assume package = base if not provided
#     if (length(pkg_page) == 1L)
#       pkg_page <- c("base", pkg_page)
#     cli::style_hyperlink(topic, "ide:help",
#       params = c(package = pkg_page[1], topic = pkg_page[2]))
#   } else {
#     # Construct an https link like...
#     # "<https://davidgohel.github.io/flextable/reference/flextable.html>"
#     # for terminals that support hyperlinks like the Gnome terminal
#     # For now, just return topic (no hyperlink)
#     paste0("?", topic)
#   }
# }

# This is the new experimental subsettable_type fun$type()
# Until now, we had a very basic support of fun$type() construct. This is an
# attempt to elaborate to allow:
# - adding more types
# - having completion after fun$...
# - having completion of functions arguments, depending on the selected type
#   (to avoid putting all arguments for all types together, otherwise, it is
#   a mess)

# TODO: we should give an object instead of method, and it finds it
# TODO: also old switch() approach + hybrid
# TODO: resolve .?fun$type(obj) to the right page + explanations
name_function_type <- function(fun, method = NULL, type) {
  stopifnot(is.character(fun), is.character(type),
    length(fun) == 1, length(type) == 1)
  if (is.null(method)) {
    paste0(fun, "_", type)
  } else {# Method provided
    stopifnot(is.character(method), length(method) == 1)
    paste0(fun, ".", method, "_", type)
  }
}

list_types <- function(fun, method = NULL) {
  fun_search <- name_function_type(fun, method, "")
  #fun_search <- sub("\\$", "\\\\$", fun_search)
  fun_search <- gsub("\\.", "\\\\.", fun_search)
  fun_search <- paste0("^", fun_search)
  res <- apropos(fun_search, mode = "function")
  sub(paste0(fun_search, "(.+)$"), "\\1", res)
}

get_type <- function(fun, method = NULL, type, stop.if.missing = TRUE) {
  fun_name <- name_function_type(fun, method, type)
  fun_body <- get0(fun_name, envir = parent.frame(), mode = "function")
  if (is.null(fun_body) && isTRUE(stop.if.missing)) {
    types <- list_types(fun, method)
    if (is.null(method)) {
      fun_str <- paste0(fun, "()")
    } else {
      fun_str <- paste0(fun, ".", method, "()")
    }
    if (!length(types)) {
      stop("Type '", type, "' not found for function ", fun_str,
        " (no type found for this function).")
    } else {
      stop("Type '", type, "' not found for function ", fun_str,
        ". Known types: ", paste(types, collapse = ", "))
    }
  }
  fun_body
}

args_type <- function(fun, method = NULL, type) {
  fun_name <- name_function_type(fun, method, type)
  fun_body <- get0(fun_name, envir = parent.frame(), mode = "function")
  if (is.null(fun_body)) {
    if (is.null(method)) {
      fun_str <- paste0(fun, "()")
    } else {
      fun_str <- paste0(fun, ".", method, "()")
    }
    stop("Type '", type, "' not found for function ", fun_str)
  }
  args(fun_body)
}

# In {chart} the old function was defined as:
#`$.subsettable_type` <- function(x, name)
#  function(...) x(type = name, ...)
#
# Now, it is a little bit more complex because we try first to call a function
# with name.<generic>$type()
`$.subsettable_type` <- function(x, name) {
  cl <- class(x)
  generic <- cl[length(cl)] # Last item
  fun_type <- paste0(generic, "_", name)
  fun <- get0(fun_type, envir = parent.frame(), mode = "function")
  if (is.null(fun)) {# Old behaviour: recall the original function with type arg
    function(...) x(type = name, ...)
  } else {
    fun
  }
}

# This is how completion is done for fun$
# TODO: get completion for regular fun$types + hybrid mode
.DollarNames.subsettable_type <- function(x, pattern = "") {
  cl <- class(x)
  generic <- cl[length(cl)] # Last item
  res <- list_types(generic)
  if (!length(res))
    return("")
  res <- res[grepl(pattern, res)]
  # Indicate that these are functions by adding '('
  paste0(res, "(")
}

# Example
# Our subsettable functions are of class subsettable_type, function, <generic>
# where <generic> is the name of the generic
# Example
#head2 <- structure(function(data, n = 10, ..., type = "default") {
#  # This was the old (static) aaproach: not possible to add a new type
#  # without modifying the funxction head2()
#  #switch(type,
#  #  default = `.head2$default`(data, n = n, ...),
#  #  fun = `.head2$fun`(data, n = n, ...)
#  #)
#  # This is the new (dynamic) approach
#  get_type("head2", type = type)(data, n = n, ...)
#}, class = c("subsettable_type", "function", "head2"))

# We define two types for head2(): default and fun
#`head2_default` <- function(data, n = 10, ...)
#  head(data, n = n)

# Apply a fun on head() - just an example, not necessarily useful
#`head2_fun` <- function(data, n = 10, fun = summary, ...)
#  head(data, n = n) |> fun(...)

#head2(iris)
#head2(iris, type = "default") # Idem
#head2$default(iris) # Idem
#head2$fun(iris) # The other type, with fun = summary()
#head2$fun(iris, fun = str)

# Now, the completion in RStudio
# 1. Type head2$ and you got the list of available types
# 2. Select "default" then hit <tab>, you got the list of args for default
# 3. Do the same but select "fun", now you got the arguments for the fun type
# 4. Just write a new `.head2_<type>` function and <type> is automatically
#    integrated!

# This function handles duplicates in a vector of labels. It either replaces
# duplicates with their names or adds a number to the end of each duplicate
# element. If replace_with_name is TRUE (default), duplicates are replaced with
# their names.

.handle_duplicate_labels <- function(x, replace_with_name = TRUE) {
  # Check for duplicates
  #duplicates  <- collapse::fduplicated(x)
  duplicates <- duplicated(x) | duplicated(x, fromLast = TRUE)

  if(any(duplicates)) {
    warning(
      "Warning: There are duplicate elements in the labels.
      Tip: Use data.io::labelise() to replace your duplicate labels.")

    if(replace_with_name) {
      # Replace duplicates with their names
      x[duplicates] <- names(x)[duplicates]
    } else {
      # Count occurrences of each element
      counts <- table(x)

      # Find duplicate elements
      duplicates <- names(counts[counts > 1])

      # For each duplicate element, add a number at the end
      for (name in duplicates) {
        index <- which(x == name)
        x[index] <- paste(x[index], "(", seq_along(index), ")", sep = "")
      }
    }
  }

  return(x)
}
