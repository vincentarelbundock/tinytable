# Make colors more uniform: R definition -> LaTeX definition -> default
standardize_colors <- function(col, format = "hex") {
  if (length(col) == 1 && is.na(col)) {
    return(NA)
  }

  normalize <- isTRUE(getOption("tinytable_color_name_normalization", default = TRUE))
  if (!normalize) {
    return(col)
  }

  single_color <- function(k) {
    # NA means "no color"; do not let it reach col2rgb() (which returns white)
    if (is.na(k)) {
      return(NA_character_)
    }

    # Early return for hex colors
    # do this here to allow typst rgb() wrap later
    if (isTRUE(grepl("^#", k))) {
      return(k)
    }

    # Skip processing for "black" and "white"
    if (k %in% c("black", "white")) {
      return(k)
    }

    # R colors
    out <- try(grDevices::col2rgb(k), silent = TRUE)

    if (!inherits(out, "try-error")) {
      # Convert RGB values to hex format
      out <- grDevices::rgb(out[1], out[2], out[3], maxColorValue = 255)
    } else if (format != "tabularray" && k %in% latex_colors$name) {
      # LaTeX colors (skip for tabularray format)
      hex_val <- latex_colors$rgb[latex_colors$name == k]
      out <- paste0("#", hex_val)
    } else {
      # If still no match, return the original value
      out <- k
    }
    return(out)
  }
  result <- sapply(col, single_color)

  # Format for Typst if needed
  if (format == "typst") {
    result <- ifelse(
      is.character(result) & grepl("^#", result),
      sprintf('rgb("%s")', result),
      result
    )
  }

  # Format for ANSI if needed
  if (format == "ansi") {
    format_ansi_color <- function(color_val) {
      if (is.na(color_val)) {
        return(NA)
      }

      # If it's a hex color, convert to RGB ANSI
      if (grepl("^#[0-9A-Fa-f]{6}$", color_val)) {
        hex_color <- substr(color_val, 2, 7) # Remove #
        r <- as.integer(paste0("0x", substr(hex_color, 1, 2)))
        g <- as.integer(paste0("0x", substr(hex_color, 3, 4)))
        b <- as.integer(paste0("0x", substr(hex_color, 5, 6)))
        return(sprintf("38;2;%d;%d;%d", r, g, b))
      }

      # Check if it's a named ANSI color
      ansi_colors <- list(
        "black" = "30", "red" = "31", "green" = "32", "yellow" = "33",
        "blue" = "34", "magenta" = "35", "cyan" = "36", "white" = "37",
        "gray" = "90", "grey" = "90", "bright_red" = "91", "bright_green" = "92",
        "bright_yellow" = "93", "bright_blue" = "94", "bright_magenta" = "95",
        "bright_cyan" = "96", "bright_white" = "97"
      )

      color_code <- ansi_colors[[tolower(color_val)]]
      if (!is.null(color_code)) {
        return(color_code)
      }

      # If no match, return original
      return(color_val)
    }

    result <- sapply(result, format_ansi_color, USE.NAMES = FALSE)
  }

  return(result)
}


#' Build a named map from unique user-supplied colors to normalized values
#'
#' Calls standardize_colors() once per unique non-NA value, in order of first
#' appearance. Callers that need the raw map (e.g. for later name lookups or
#' to register LaTeX preamble entries) use this directly; others go through
#' normalize_colors().
#'
#' @param values Character vector of user-supplied colors (may contain NA)
#' @param format Passed to standardize_colors ("hex", "tabularray", "typst")
#' @return Named character vector: original color -> normalized color
#' @keywords internal
#' @noRd
build_color_map <- function(values, format) {
  vals <- unique(values[!is.na(values)])
  if (length(vals) == 0) {
    return(character(0))
  }
  stats::setNames(
    sapply(vals, standardize_colors, format = format, USE.NAMES = FALSE),
    vals
  )
}

#' Apply a prebuilt color map to a vector of colors
#'
#' @param values Character vector of user-supplied colors (may contain NA)
#' @param map Named character vector from build_color_map()
#' @param default Value used for NA and unmatched entries
#' @param preserve_css_vars Keep unmatched var(--...) values untouched
#'   (used by the HTML lines path)
#' @return Character vector of normalized colors
#' @keywords internal
#' @noRd
apply_color_map <- function(values, map, default = "black", preserve_css_vars = FALSE) {
  out <- rep(default, length(values))
  idx <- which(!is.na(values) & values %in% names(map))
  out[idx] <- unname(map[values[idx]])
  if (preserve_css_vars) {
    idx <- which(!is.na(values) & !(values %in% names(map)) & grepl("^var\\(", values))
    out[idx] <- values[idx]
  }
  out
}

#' Normalize a vector of user-supplied colors via a unique-value map
#'
#' Convenience wrapper: build_color_map() + apply_color_map().
#'
#' @inheritParams apply_color_map
#' @param format Passed to standardize_colors ("hex", "tabularray", "typst")
#' @keywords internal
#' @noRd
normalize_colors <- function(values, format, default = "black", preserve_css_vars = FALSE) {
  apply_color_map(
    values,
    build_color_map(values, format),
    default = default,
    preserve_css_vars = preserve_css_vars
  )
}
