# Shared CSS template for the revealjs theme. Placeholders (in order):
# body font size, header/last-row border color, caption color, caption font size.
css_revealjs_template <- "
/* tables */

.reveal table {
  /* height: auto; Adjust table width to fit content up to the available slide space */
  margin: auto;
  border-collapse: collapse;
  border-spacing: 0;
}

.reveal table th,
.reveal table td {
  border: none;
  padding: .23em;
  font-size: %sem;
}

/* Adds a bottom border to the table header row for distinction */
.reveal table thead th,
.reveal .slides table tr:last-child td,
.reveal .slides table {
  border-bottom: 2px solid %s;
}

/* Make column headers bold */
.reveal table thead th {
}

/* Styling table captions */
.reveal table caption {
  color: %s;
  font-size: %sem;
}
"

# Extra rules appended for the dark variant only.
css_revealjs_dark_addendum <- "
.reveal table {
  background-color: #2d2d2d; /* Dark background for tables */
  color: white; /* White text color */
}

.reveal table th,
.reveal table td {
  color: white; /* White text color */
}
"

#' RevealJS presentation theme
#'
#' @param x A `tinytable` object.
#' @param css String. CSS theme: "light" (default) or "dark".
#' @param fontsize Numeric. Font size multiplier for table content.
#' @param fontsize_caption Numeric. Font size multiplier for table captions.
#' @return A modified `tinytable` object.
#' @export
theme_revealjs <- function(
  x,
  css = get_option("tinytable_revealjs_css", default = "light"),
  fontsize = get_option("tinytable_revealjs_fontsize", default = 0.8),
  fontsize_caption = get_option("tinytable_revealjs_fontsize_caption", default = 1)) {
  if (css == "light") {
    css <- sprintf(
      css_revealjs_template,
      format_markup_num(fontsize),
      "#D3D3D3",
      "#666666", # Dark grey color for the caption
      format_markup_num(fontsize_caption)
    )
  } else if (css == "dark") {
    css <- paste0(
      sprintf(
        css_revealjs_template,
        format_markup_num(fontsize),
        "white", # White border color for dark theme
        "white", # White color for the caption
        format_markup_num(fontsize_caption)
      ),
      css_revealjs_dark_addendum
    )
  }
  x <- theme_html(x, engine = "tinytable", css_rule = css)
  return(x)
}
