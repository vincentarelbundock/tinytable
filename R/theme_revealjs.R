theme_revealjs <- function(
    x,
    fontsize = get_option("tinytable_theme_revealjs_fontsize", default = 0.8),
    fontsize_caption = get_option("tinytable_theme_revealjs_fontsize_caption", default = 1)) {
  css <- "
// tables

.reveal table {
  // height: auto; /* Adjust table width to fit content up to the available slide space */
  margin: auto;
  border-collapse: collapse;
  border-spacing: 0;
}

.reveal table th,
.reveal table td {
  border: none;
  padding: .23em;
  font-weight: lighter;
  font-size: %sem;
}

/* Adds a bottom border to the table header row for distinction */
.reveal table thead th,
.reveal .slides table tr:last-child td,
.reveal .slides table {
  border-bottom: 2px solid #D3D3D3;
}

/* Make column headers bold */
.reveal table thead th {
/*  font-weight: bold; */
}

/* Styling table captions */
.reveal table caption {
  color: #666666; /* Dark grey color for the caption */
  font-size: %emem;
}
"
  css <- sprintf(css, fontsize, fontsize_caption)
  x <- style_tt(x, bootstrap_css_rule = css)
  return(x)
}
