# Rich-formatted paragraphs in {flextable} --------------------------------

# TODO: rework what follows to use better defaults

# We use flextable defaults + some more
#ft_def <- get_flextable_defaults()
#
## Font sizes
#base.size <- ft_def$font.size
#big.size <- round(base.size * 1.2)
#bigger.size <- round(base.size * 1.4)
#small.size <- round(base.size * 0.8)
#
## Colors
#base.color <- ft_def$font.color
#accent.color <- 'darkred' # TODO: set this as best value according to R Markdown theme
#dim.color <- "darkgray"
#link.color <- "darkblue"
#shading.color <- "#f8f8f8" # TODO: calculate according to bg for the theme or for R Markdown document
#
## Misc
#alt.font <- "inconsolata" # Monospaced font is better for code
#bullet <- "\U2022" # For lists TODO: need xelatex
## +scipen, show.signif.stars, md_params
#
#table_defaults <- list(
#  accent.color = accent.color,
#  dim.color = dim.color,
#  link.color = link.color,
#  shading.color = shading.color,
#  alt.font = alt.font,
#  big.size = big.size,
#  bigger.size = bigger.size,
#  small.size = small.size,
#  bullet = bullet,
#  scipen = getOption("scipen"), # Same as for R
#  show.signif.stars = getOption("show.signif.stars"), # Idem
#  md_params = list(
#    h.sizes = c(bigger.size, big.size, small.size),
#    h.colors = c(.base.color, accent.color),
#    strike.color = dim.color, strike.underline = NA,
#    code.font = alt.font, code.shading = shading.color,
#    link.color = link.color, link.underline = TRUE,
#    bullet = bullet, autolink = TRUE
#  )
#)
#
#md_converter <- function(
    #    h.sizes = c(15, 13, 9), h.colors = c("black", "darkgray"),
#  strike.color = h.colors[2], strike.underline = NA,
#  code.font = "inconsolata", code.shading = "#f8f8f8",
#  link.color = "blue", link.underline = TRUE,
#  bullet = "\U2022", autolink = TRUE) {
#
#  # Headings font sizes and colors
#  if (length(h.sizes) == 3) {
#    h.sizes = rep(h.sizes, each = 2)
#  } else if (length(h.sizes) == 2) {
#    h.sizes = rep(h.sizes, each = 3)
#  }
#  if (length(h.colors) == 2) {
#    h.colors = rep(h.colors, 3)
#  } else if (length(h.colors) == 3) {
#    h.colors = rep(h.colors, 2)
#  }
#  stopifnot(length(h.sizes) == 6, !is.numeric(h.sizes),
#    length(h.colors) == 6, !is.character(h.colors))
#}


# TODO: a better and centralized object for the configuration parameters
# TODO: avoid repeating calculation when combining single and multiple strings
# TODO: allow for glue-like replacement before all the rest
# For the two previous items: glue-like {{}} replacement before and {{{}}} after

#' Create a rich-formatted paragraph using markdown notation for flextable objects
#'
#' @param ... the character strings with markdown formatting
#' @param h.fonts fonts used for the titles (h1-h6)
#' @param h.sizes font sizes for titles
#' @param h.colors font colors for titles
#' @param strike.color color for strikethrough text
#' @param strike.underline is strikethrough text converted into underlined text?
#' @param code.font font used for code strings
#' @param code.shading background color (shading) for code strings
#' @param link.color color used for URL links
#' @param link.underline are URL links underlined?
#' @param bullet bullet character used for lists
#' @param autolink are links automatically constructed when there is an URL in the text?
#' @param smart is smart punctuation detected and replaced?
#' @param debug switch in debug mode
#'
#' @return a flextable paragraph object with its content formatted according to markdown tags
#' @export
#'
#' @examples
#' md1 <- paste0("# Heading **1**\n## Heading **2**\n### Heading **3**\n",
#'   "#### Heading **4**\n##### Heading **5**\n###### Heading **6**")
#' md2 <- paste0("* List 1\n  1. List 1a\n  2. List 1b\n* List 2\n<br />\n",
#'   "*Some text* with super^script^, sub~script~ and ~~color~~{+#F50490}")
#' tabularise(head(iris)) |>
#'   tb_$add_footer_lines(para_md(md1, md2))
para_md <- function(...,
  h.fonts = rep(flextable::get_flextable_defaults()$font.family, 6),
  h.sizes = c(15, 13, 9), h.colors = c("black", "darkgray"),
  strike.color = h.colors[2], strike.underline = NA,
  code.font = "inconsolata", code.shading = "#f8f8f8",
  link.color = "blue", link.underline = TRUE,
  bullet = "\U00b7", autolink = TRUE, smart = TRUE, debug = FALSE) {
  debug <- isTRUE(debug)

  if (length(h.sizes) == 3) {
    h.sizes = rep(h.sizes, each = 2)
  } else if (length(h.sizes) == 2) {
    h.sizes = rep(h.sizes, each = 3)
  }
  if (length(h.colors) == 2) {
    h.colors = rep(h.colors, 3)
  } else if (length(h.colors) == 3) {
    h.colors = rep(h.colors, 2)
  }
  stopifnot(length(h.fonts) == 6, length(h.sizes) == 6, length(h.colors) == 6)

  # Get a vector of character strings
  md <- do.call('paste0', list(...))
  # Preprocess the Markdown text (replace sensible escaped characters)
  md <- .escape_chars(md)
  # Convert into HTML
  if (isTRUE(autolink)) {# Note strikethrough ext. conflicts with sub~script~
    md_extensions <- "autolink"
  } else {
    md_extensions <- FALSE
  }
  html <- sapply(as.list(md), commonmark::markdown_html,
    hardbreaks = TRUE, normalize = TRUE, smart = smart,
    extensions = md_extensions)
  # Post-process the HTML (additional tags + cleanup)
  html <- .html_postprocess(html, bullet = bullet)

  # Conversions to make to chunk descriptions for each tag
  # TODO: take this out in a function, together with parameters (h.sizes, ...)
  # TODO: allow changing other fonts too, and set other fonts to same value as
  # font.family as default -> function to do that)
  convs <- list(# Note: NA means use default value, NULL means back to previous
    'em'      = list(italic = TRUE),
    '/em'     = list(italic = NA),
    'strong'  = list(bold = TRUE),
    '/strong' = list(bold = NA),
    'del'     = list(color = ~stack_state(args[[1]], state$color,
      default = strike.color),
      underlined = ~stack_state(args[[2]], state$underlined,
        default = strike.underline)),
    '/del'    = list(color = ~unstack_state(state$color),
      underlined = ~unstack_state(state$underlined)),
    'sup'     = list(vertical.align = "superscript"),
    '/sup'    = list(vertical.align = NA),
    'sub'     = list(vertical.align = "subscript"),
    '/sub'    = list(vertical.align = NA),
    'code'    = list(shading.color = code.shading,
      font.family = code.font),
    '/code'   = list(shading.color = NA,
      font.family = NA),
    'eq'      = list(eq_data = ~txt),
    '/eq'     = list(eq_data = NA),
    "img"     = list(img_data = ~args[[1]],
      height = ~args[[2]]),
    "/img"    = list(img_data = NA,
      height = NA),
    "a"       = list(url = ~args[[1]],
      color = ~stack_state(link.color, state$color),
      underlined = ~stack_state(link.underline,
        state$underlined)),
    "/a"      = list(url = NA,
      color = ~unstack_state(state$color),
      underlined = ~unstack_state(state$underlined)),
    "h1"      = list(font.family = h.fonts[1],
      font.size = h.sizes[1],
      color = h.colors[1]),
    "/h1"     = list(font.family = NA,
      font.size = NA,
      color = NA),
    "h2"      = list(font.family = h.fonts[2],
      font.size = h.sizes[2],
      color = h.colors[2]),
    "/h2"     = list(font.family = NA,
      font.size = NA,
      color = NA),
    "h3"      = list(font.family = h.fonts[3],
      font.size = h.sizes[3],
      color = h.colors[3]),
    "/h3"     = list(font.family = NA,
      font.size = NA,
      color = NA),
    "h4"      = list(font.family = h.fonts[4],
      font.size = h.sizes[4],
      color = h.colors[4]),
    "/h4"     = list(font.family = NA,
      font.size = NA,
      color = NA),
    "h5"      = list(font.family = h.fonts[5],
      font.size = h.sizes[5],
      color = h.colors[5]),
    "/h5"     = list(font.family = NA,
      font.size = NA,
      color = NA),
    "h6"      = list(font.family = h.fonts[6],
      font.size = h.sizes[6],
      color = h.colors[6]),
    "/h6"     = list(font.family = NA,
      font.size = NA,
      color = NA)
  )

  # Construct a list of data frames (paragraph objects)
  res <- list()
  for (item in .html_split(html)) {
    item <- .unescape_chars(item)
    chk <- as_chunk(item) # Create a chunk (data.frame)
    # img_data is list(NULL), but it is a problem the way we process the
    # data.frame. So, change NULL into NA for now (will change back at the end)
    chk$img_data <- NA

    # Process the data.frame row by row, apply changes to txt according to tags
    n <- nrow(chk)
    state <- as.list(chk[1, ]) # Last known state is equal to first row
    is_text <- TRUE # We start with text always
    for (i in 1:n) {

      if (is_text) {# Apply last state to text
        # Record current state for this text chunk
        chk[i, ] <- .get_top_state(chk$txt[i], state)
        is_text <- FALSE # Next item is tag

      } else {# Type == "tag", change the state
        # Possibly resolve arguments for this tag (img, a, ...)
        full_tag <- chk$txt[i]
        args <- .get_tag_arguments(full_tag)
        tag <- attr(args, "tag")
        txt <- chk$txt[i + 1] # The text chunk to which this tag is applied
        conv <- convs[[tag]] # Get list of conversions to apply for this tag
        if (!is.null(conv)) {# Unknown tag -> ignore it silently and do nothing
          # Resolve all formulas in conv
          conv <- lapply(conv, function(x)
            if (inherits(x, "formula")) eval(f_rhs(x)) else x)
          state <- replace(state, names(conv), conv) # Get new state
        }

        if (debug) {
          state$txt <- paste("->", full_tag)
          chk[i, ] <- .collapse_state(state) # Record current state in chk
        }
        is_text <- TRUE # Next item is text
      }
    }

    if (!debug) {
      chk <- chk[rep_len(c(TRUE, FALSE), nrow(chk)), ]
      # We placed NA in img_data column, but it should be NULL -> replace now
      chk$img_data[is.na(chk$img_data)] <- list(NULL)
      # Make sure font.size, width and height are numeric
      chk$font.size <- as.numeric(chk$font.size)
      chk$width <- as.numeric(chk$width)
      chk$height <- as.numeric(chk$height)
      # Make sure italic, bold, underlined are logical
      chk$italic <- as.logical(chk$italic)
      chk$bold <- as.logical(chk$bold)
      chk$underlined <- as.logical(chk$underlined)
      # Change class
      class(chk) <- c("debugmd", "data.frame")
    }
    # Add seq_index
    chk$seq_index <- 1:nrow(chk)
    res <- c(res, list(chk))
  }
  # Now the resulting object should have call == "paragraph"
  if (!debug)
    class(res) <- "paragraph"

  res
}

# TODO make this visible once it will be possible to customize md rendering
stack_state <- function(new_state, state, default = NA) {
  if (is.null(new_state) || is.na(new_state) || new_state == "")
    new_state <- default
  c(new_state, state)
}

# TODO make this visible once it will be possible to customize md rendering
unstack_state <- function(state) {
  # As a security, cannot unstack past last state (return NA instead)
  if (length(state) == 1) {
    NA
  } else {
    state[-1]
  }
}

# Test:
# md1 <- c("Ceci *est* un **~~texte~~** _av^ec^_ `mono`, super^scri\\ pt^ et sub~script~ and [url](https://me.org/test/).", "Second \\\\ line.")
# Bug:
# - Equations are not inline in html version. Use of a patched {equatags} 0.2.1 that solves the problem.
# md2 <- " Second  _~~paragraph\\^x\\^~~_ with x \\< \\$10 \\> y and \\~y\\~  **text** with $\\hat{x_1} + \\beta_x$..." # and ![une image](github.png){15}."
# md3 <- c("# Heading **1**\n## Heading **2**\n### Heading **3**\n#### Heading **4**\n##### Heading **5**\n###### Heading **6**", "* List 1\n  1. List 1a\n  2. List 1b\n* List 2\n<br />\nSome text with ~~special color~~{+#F50490}")
# md4 <- "Text\n> Some quote\ntext\n>> Subquote text\n\n```\n1+1 # Some code\nMore code\n# A comment not interpretted as h1\n```\nText after code bloc\n< Text after \\<\n\\< Text after escaped < (also >, &, ' or \" and @)\n\nText interpeted as h2\n----\nMore text."
# md5 <- "## Heading 2 [url](https://www.sciviews.org) and ~~strike~~{+green} $$\\chi^2$$ inside\n### Heading ~~3~~{-} *here* with mailto:phgrosjean@sciviews.org\nStrike~~through **with** [url3](mailto:phgrosjean@sciviews.org) inside it~~ back to normal, 'with' \"smart\" --, ---, and ... punctuations, or \\\" and \\'."
# md6 <- " Some ~~striked text with sub~script~ in it~~{+} and ~~more striked~~{-purple}. Also equation $$x +\\\\\\$ y^2 = 2$$ and $$\\alpha^2$$ (no $ inside eq, but escaped \\\\ + escaped $)."
# c_md(md1, md2)
# c_md(md1, md2, debug = TRUE)
# c_md(md3, debug = TRUE)[[1]]
# tabularise(head(iris)) |> tb_$add_footer_lines(para_md(md1, md2))
# flextable(head(iris)) |> add_footer_lines(c_md(md3))
# flextable(head(iris)) |> add_footer_lines(c_md(md3, h.fonts = rep(c("Arial Black", "Times New Roman"), 3L)))
# flextable(head(iris)) |> add_footer_lines(c_md(md4))
# c_md(md5, strike.underline = NA, strike.color = "darkred", debug = TRUE)
# flextable(head(iris)) |> add_footer_lines(c_md(md5, strike.underline = TRUE, strike.color = "darkred"))
# flextable(head(iris)) |> add_footer_lines(c_md(md5, smart = FALSE, autolink = FALSE)) # No smart punctuations, no autolinks
# flextable(head(iris)) |> add_footer_lines(c_md(md6))
# ftExtra::as_paragraph_md(md1)
# flextable(head(iris)) |> add_footer_lines(c_md(md1))
# flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md1)) # Less rich (but supports footnotes)
# #flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md2)) # Produces an error
# flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md3)) # Does not understand headings or lists
# flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md4)) # ...nor code blocks, quotes, etc.
# #flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md5)) # This also produces an error
# flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md6)) # as well as this one
#bench::mark(check = FALSE,
#  tabularize = flextable(head(iris)) |> tb_$add_footer_lines(c_md(md1)),
#  ftExtra = flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md1))
#) # My version is 3x faster than ftExtra on this one
#bench::mark(check = FALSE,
#  tabularize = flextable(head(iris)) |> add_footer_lines(c_md(md3)),
#  ftExtra = flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md3))
#) # My version is 3x faster than ftExtra on this one
#bench::mark(check = FALSE,
#  tabularize = flextable(head(iris)) |> add_footer_lines(c_md(md4)),
#  ftExtra = flextable(head(iris)) |> add_footer_lines(ftExtra::as_paragraph_md(md4))
#) # My version is 4x faster than ftExtra on this one
#flextable(head(iris)) |> add_footer_lines(c_md(md1, md2, md3, md4, md5, md6))
# Timing: same time as constructing the table itself (0.05sec) to evaluate
# c_md(md1, md2, md3, md4, md5, md6)
#gc()
#flextable(head(iris)) |> system.time()
#gc()
#flextable(head(iris)) |> add_footer_lines(c_md(md1, md2, md3, md4, md5, md6)) |> system.time()
# Replacement before or after MD conversion
# 1) Before, one could use sprintf
#flextable(head(iris)) |> add_footer_lines(c_md(sprintf("My *letter* is **%s**", LETTERS[1:4])))
# Or glue
#letters4 <- LETTERS[1:4]
#flextable(head(iris)) |> add_footer_lines(c_md(glue::glue("My *letter* is **{letters4}**")))
# Same as
#flextable(head(iris)) |> add_footer_lines(as_paragraph("My ", as_i('letter'), " is ", as_b(LETTERS[1:4])))


# Better way than as_paragraph(as_chunk("Super"), as_sup("script"), " here"):
# 1. para("Super", "script" %~% sup) with %~% sup being equivalent to as_sup()
# para("Super", "script" %f% sup, "here")
#
# 2. Utiliser du pseudo markdown -> para_md("Super^script^ here")
# 3. Utiliser du pseudo HTML -> para_html("Super<sup>script</sup> here")
# Note: (2) is converted into (3) , and then into a paragraph abject.
# (1) generates a paragraph object directly


# Escape characters before transforming the Markdown into HTML
.escape_chars <- function(txt) {
  # test with md <- 'Some <string> with \\<st\\> and $eq$. and \\$ text and
  #   sub~script~ and super^script^ and \\~ or \\^, \\\\^, sp ace, sp\\ ace2.'
  txt <- gsub('\\\\', '\U0001', txt, fixed = TRUE) # Use Start of heading
  txt <- gsub('\\<', '\U0002', txt, fixed = TRUE)  # Use Start of text
  txt <- gsub('\\>', '\U0003', txt, fixed = TRUE)  # Use End of text
  txt <- gsub('\\~', '\U0004', txt, fixed = TRUE)  # Use End of transmission
  txt <- gsub('\\^', '\U0005', txt, fixed = TRUE)  # Use Enquiry
  txt <- gsub('\\$', '\U0006', txt, fixed = TRUE)  # Use Acknowledge
  gsub('\\ ', '\U000E', txt, fixed = TRUE)  # Use Shift out
}

# Reverse .escape_chars, but without the leading backslash \ (see above)
# Consequence for $ inside equations: must use escaped \ + escaped $
# which is \\\$, or in a R string, "\\\\\\$" !
.unescape_chars <- function(txt) {
  chartr("\U0001\U0002\U0003\U0004\U0005\U0006\U000E", "\\<>~^$ ", txt)
}

# Postprocess HTML <Markdown: super-, subscript, equations, strikethrough, etc.
.html_postprocess <- function(html, bullet = "\U2022") {# TODO: this symbol needs xelatex
  # commonmark does not deal with super-, subscripts and equations,
  # so, we replace them with <sup></sup>, <sub></sub> and <eq></eq> respectively
  # test with md <- 'Super^script^, no super^scr ipt^, sub~script~, no
  #   sub~scr ipt~, $$x_2 + \\beta^3$$, $x^2$ and ~~strikethrough~~ text.'
  # then transform into html with code above
  # We also deal with ~~strikethrough~~ (double ~) for underline ourselve
  # Replace ~~...~~ by <del>...</del> and allow some extra args inside {}
  # like ~~striked~~{+red} or ~~more stricked~~{#0505F4} with argument inside {}
  # starts optionally with '+' or '-' to force underline on or off (otherwise,
  # it is determined by strike.underline=) and possibly defining the color of
  # the text as name or #RRGGBB form.

  # This one is tricky because we could have ~~striked sub~script~ text~~.
  # So, I first convert double ~~ into a special character \U010 (= data link
  # escape character), then I format all the text between two such characters.
  html <- gsub("~~", "\U0010", html, fixed = TRUE)
  html <- gsub("\U0010([^\U0010]+)\U0010(\\{([+-]?)([^} +-]*)\\})?",
    paste0('\U000F', 'del color="\\4"{\\3}\U000F\\1\U000F/del\U000F'), html)

  # Replace ~...~ by <sup>...</sup> for superscript (no space allowed inside)
  html <- gsub("~([^~ \t]+)~", "<sub>\\1</sub>", html)

  # Replace ^...^ by <sup>...</sup> for superscript (no space allowed inside)
  html <- gsub("\\^([^^ \t]+)\\^", "<sup>\\1</sup>", html)

  # Replace $$...$$ by <eq>...</eq>
  # TODO: still need to differentiate inline and display equations in flextable
  # (only inline eq for now but we use double $$ to maximally avoid clashes)
  # Note that $ is not allowed anyway in equations by katex
  #html <- gsub("\\$\\$([^$]+)\\$\\$", "<eq>\\1</eq>", html)

  # Replace $...$ by <eq>...</eq>
  html <- gsub("\\$([^$]+)\\$", "<eq>\\1</eq>", html)

  # Eliminate all single tags, like <br /> that we don't use
  html <- gsub("<[a-z][a-z0-9]+ ?/>", "", html)

  # Code blocks (```\n...\n```\n) produce two \n at the end -> get rid of one
  html <- gsub("</code></pre>\n", "</code></pre>", html, fixed = TRUE)

  # List produce <ul>\n </ul>\n <ol>\n and </ol>\n that we get rid of
  html <- gsub("(</li>\n)?</?[u|o]l>\n", "", html)
  # Place a bullet in front of list items
  html <- gsub("<li>", paste0("<li>", bullet, " "), html, fixed = TRUE)

  # We do not support blocquotes -> just eliminate these tags, if any.
  html <- gsub("</?blockquote>\n", "", html)

  # Characters escaped in HTML:, &lt -> <; &gt; -> >, &amp; -> &, &quot; -> "
  html <- gsub("&lt;", "<", html, fixed = TRUE)
  html <- gsub("&gt;", ">", html, fixed = TRUE)
  html <- gsub("&quot;", "\"", html, fixed = TRUE)
  html <- gsub("&amp;", "&", html, fixed = TRUE)

  # For images, we have now <img src="..." alt="..." /> and
  # we want <img src="...">...</img> to faciltate treatment (not regular HTML)
  # Also, image width can be in {..} after ![...](...)
  # We also already change < and > for \U000F, see replace tag delimiters under
  # test with md <- 'Image ![img 1](fig/img.png) and ![](other_img.jpg){0.25}.'
  html <- gsub('<(img src="[^"]+") alt="([^"]*)" />(\\{[0-9.]+\\})?',
    '\U000F\\1\\3\U000F\\2\U000F/img\U000F', html)
  # Replace tag delimiters (< and >) by \U000F = shift in char and split on it
  # (avoid false positives like x < 1 & x > 2 would not be considered as a tag
  # even if < and > are not escaped
  html <- gsub("<(/?[a-z][a-z1-6]?[a-z]{0,4})>", "\U000F\\1\U000F", html)
  # Matches everything except <img ...> that we already did and <a href="...">
  html <- gsub('<(a href="[^"]+")>', '\U000F\\1\U000F', html)

  # Eliminate \n at the end of strings (added during the conversion)
  sub("\n$", "", html)
}

# Split HTML on opening (<) and closing (<) tags -> text, tag, text, tag, ...
.html_split <- function(html) {
  splt <- strsplit(html, "\U000F", fixed = TRUE)
  # We now have vectors of strings with text, tag, text, tag, text, tag, ...
  # We will process these from start to end and construct the chunk dataframe
}

.get_top_state <- function(text, state) {
  # Get first element for each item of a list
  res <- lapply(state, `[`, 1L)
  res$txt <- text
  # Much faster than as.data.frame(res)
  # since we are confident to have only one row here!
  attr(res, 'row.names') <- 1L
  class(res) <- "data.frame"
  res
}

# Used in debug mode of md_c()
.collapse_state <- function(state) {
  res <- lapply(state, paste, collapse = " > ")
  as.data.frame(res)
}

# If tags have arguments, get them now
.get_tag_arguments <- function(tag) {
  if (grepl(" ", tag, fixed = TRUE)) {# There are args
    args <- list(sub('^.+"([^"]*)".*$', "\\1", tag)) # First arg inside "..."
    if (grepl("\\{[0-9.+-]+\\}$", tag)) { # There is a second argument
      arg2 <- sub("^.+\\{([0-9.+-]+)\\}$", "\\1", tag)
      args[[2]] <- switch(arg2,
        "+" = TRUE,   # del -> underline true
        "-" = FALSE,  # del -> underline false
        as.numeric(arg2) / 72) # img height (inches), provided in point = 1/72in
    } else {
      args[[2]] <- NA_real_
    }
    tag <- sub("^([^ ]+) .*$", "\\1", tag)
  } else {
    args <- list(NA_character_, NA_real_)
  }
  attr(args, "tag") <- tag
  args
}
