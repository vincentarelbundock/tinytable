#' Striped theme with alternating row colors
#'
#' @param x A `tinytable` object.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_striped <- function(x, ...) {
  # For Tabulator, use CSS rules to create striped rows
  x <- theme_html(
    x,
    tabulator_css_rule = "$TINYTABLE_ID .tabulator-row:nth-child(odd) .tabulator-cell { background-color: #ededed !important; }"
  )

  # Stripe rows are computed at build time, after group rows have been
  # inserted by rbind_body_groupi(), so every output format (LaTeX, HTML,
  # Typst, ...) stripes exactly the same rows even when group_tt() adds rows
  # before or after theme_striped() in the pipeline. LaTeX previously used a
  # `row{even}={bg=black!5!white}` tabularray spec, which counted header and
  # group rows by absolute parity and used a slightly different gray; the
  # single explicit row set with #ededed replaces it.
  #
  # The lazy call is appended to @lazy_style directly (instead of calling
  # style_tt() inside a build_prepare() hook) so the stripe styles keep this
  # call's position in the style queue: styles applied after theme_striped()
  # must still override the stripes (see issue #531).
  cal <- as.call(list(
    function(x, ...) {
      style_tt_lazy(x, i = seq(1, nrow(x), by = 2), background = "#ededed")
    },
    x = quote(x)
  ))
  x@lazy_style <- c(x@lazy_style, list(cal))

  return(x)
}
