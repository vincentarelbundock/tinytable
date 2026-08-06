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
