# =============================================================================
# Canonical style properties and per-backend capability matrix
# =============================================================================

# Properties shared by every backend keep-list.
STYLE_PROPS_BASE <- c(
  "bold", "italic", "underline", "strikeout", "monospace", "smallcap",
  "align", "alignv", "color", "background", "fontsize", "indent"
)

# Canonical cell-style properties handled by the style pipeline
# (rectangular @style_other grid construction and resolution).
STYLE_PROPS <- c(STYLE_PROPS_BASE, "html_css", "colspan", "rowspan")

# Per-backend keep-lists passed to filter_style_other(). This is the single
# place where the backend capability matrix is declared. Do not extend a
# backend's list without implementing the corresponding rendering support.
STYLE_PROPS_HTML <- STYLE_PROPS
STYLE_PROPS_TABULARRAY <- c(STYLE_PROPS_BASE, "colspan", "rowspan")
STYLE_PROPS_TYPST <- STYLE_PROPS_BASE
STYLE_PROPS_TABULATOR <- STYLE_PROPS_BASE
# Grid (markdown) handles a smaller subset: no monospace, align, alignv, or
# fontsize. Order matters here: prepare_grid_style() also uses this vector to
# select and order the columns of the filtered style data frame.
STYLE_PROPS_GRID <- c(
  "bold", "italic", "strikeout", "underline", "smallcap", "indent",
  "color", "background", "colspan", "rowspan"
)

#' Common style_eval entry: filtered cell styles + line styles
#'
#' @param x A built tinytable object with populated @style_other/@style_lines
#' @param keep Character vector of style properties the backend renders
#' @return list(other = filtered @style_other, lines = @style_lines or NULL)
#' @keywords internal
#' @noRd
style_backend_inputs <- function(x, keep) {
  other <- filter_style_other(x@style_other, keep)
  lines <- x@style_lines
  if (nrow(lines) == 0) {
    lines <- NULL
  }
  list(other = other, lines = lines)
}


# =============================================================================
# Centralized alignment code maps shared across backends
# =============================================================================

#' Map tinytable horizontal alignment codes to backend-specific values
#'
#' Vectorized; unmatched codes (including NA) pass through unchanged.
#' All targets currently share the same map: "d" (decimal) renders as
#' "center". The LaTeX backend is intentionally absent: it keeps the raw
#' l/c/r codes and routes "d" to siunitx d-columns separately.
#'
#' @param code Character vector of alignment codes (l, c, d, r)
#' @param target Backend name: "css", "typst", or "tabulator"
#' @return Character vector of backend-specific alignment values
#' @keywords internal
#' @noRd
map_align <- function(code, target = c("css", "typst", "tabulator")) {
  target <- match.arg(target)
  map <- c(l = "left", c = "center", d = "center", r = "right")
  idx <- which(code %in% names(map))
  code[idx] <- map[code[idx]]
  code
}

#' Map tinytable vertical alignment codes to backend-specific values
#'
#' Vectorized; unmatched codes (including NA) pass through unchanged.
#' The Tabulator engine uses the "css" target (top/middle/bottom).
#'
#' @param code Character vector of vertical alignment codes (t, m, b)
#' @param target Backend name: "css", "typst", or "tabularray"
#' @return Character vector of backend-specific vertical alignment values
#' @keywords internal
#' @noRd
map_alignv <- function(code, target = c("css", "typst", "tabularray")) {
  target <- match.arg(target)
  map <- switch(target,
    css = c(t = "top", m = "middle", b = "bottom"),
    typst = c(t = "top", m = "horizon", b = "bottom"),
    tabularray = c(t = "h", m = "m", b = "f")
  )
  idx <- which(code %in% names(map))
  code[idx] <- map[code[idx]]
  code
}
