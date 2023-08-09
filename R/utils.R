# Internal function for tabularise

# Links in R terminals and RStudio console are still experimental. So, we keep
# this function internal so that adaptation does not break the package for user
.peek_help_link <- function(topic) {
  if (is_interactive() && cli::ansi_has_hyperlink_support() &&
      isTRUE(cli::ansi_hyperlink_types()$help)) { # RStudio help link
    # topic must be package::helpage
    pkg_page <- strsplit(topic, "::", fixed = TRUE)[[1]]
    # Assume package = base if not provided
    if (length(pkg_page) == 1L)
      pkg_page <- c("base", pkg_page)
    cli::style_hyperlink(topic, "ide:help",
      params = c(package = pkg_page[1], topic = pkg_page[2]))
  } else {
    # Construct an https link like...
    # "<https://davidgohel.github.io/flextable/reference/flextable.html>"
    # for terminals that support hyperlinks like the Gnome terminal
    # For now, just return topic (no hyperlink)
    paste0("?", topic)
  }
}
