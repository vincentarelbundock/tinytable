format_vector_escape <- function(vec, output = "latex", ...) {
  # Early returns for edge cases
  if (length(vec) < 1 || all(is.na(vec)) || !is.character(vec)) {
    return(vec)
  }
  if (isFALSE(output)) {
    return(vec)
  }

  # Define escape patterns for each output format
  escape_patterns <- list(
    latex = list(
      # LaTeX escaping code adapted from the `gt` package, published under MIT
      # https://github.com/rstudio/gt/
      # YEAR: 2018-2024
      # COPYRIGHT HOLDER: gt authors
      chars = c(
        "\\" = "\\textbackslash{}",
        "~" = "\\textasciitilde{}",
        "^" = "\\textasciicircum{}",
        "&" = "\\&",
        "%" = "\\%",
        "$" = "\\$",
        "#" = "\\#",
        "_" = "\\_",
        "{" = "\\{",
        "}" = "\\}"
      ),
      pattern = "[\\\\&%$#_{}~^]"
    ),
    html = list(
      chars = c(
        "&" = "&amp;",
        "<" = "&lt;",
        ">" = "&gt;"
      ),
      pattern = "[&<>]"
    ),
    typst = list(
      chars = c(
        "<" = "\\<",
        ">" = "\\>",
        "*" = "\\*",
        "_" = "\\_",
        "@" = "\\@",
        "=" = "\\=",
        "-" = "\\-",
        "+" = "\\+",
        "/" = "\\/",
        "$" = "\\$",
        "#" = "\\#",
        "[" = "\\[",
        "]" = "\\]"
      ),
      pattern = "[<>*_@=+/\\$#\\[\\]\\-]"
    )
  )

  out <- vec

  if (isTRUE(output == "latex")) {
    out <- apply_escape_pattern(out, escape_patterns$latex)
  } else if (isTRUE(output == "html")) {
    out <- apply_escape_pattern(out, escape_patterns$html)
  } else if (isTRUE(output == "typst")) {
    out <- apply_escape_pattern(out, escape_patterns$typst)
  }

  return(out)
}

# Helper function to apply escape patterns
apply_escape_pattern <- function(vec, pattern_info) {
  na_out <- is.na(vec)
  if (all(na_out)) {
    return(vec)
  }

  # Short circuit if no special characters found (performance optimization)
  if (!any(grepl(pattern_info$pattern, vec[!na_out], perl = TRUE))) {
    return(vec)
  }

  m <- gregexpr(pattern_info$pattern, vec[!na_out], perl = TRUE)
  special_chars <- regmatches(vec[!na_out], m)
  escaped_chars <- lapply(special_chars, function(x) {
    pattern_info$chars[x]
  })
  regmatches(vec[!na_out], m) <- escaped_chars

  return(vec)
}
