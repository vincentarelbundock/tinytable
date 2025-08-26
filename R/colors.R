# Make colors more uniform: R definition -> LaTeX definition -> default
standardize_colors <- function(col, format = "hex") {
  if (length(col) == 1 && is.na(col)) {
    return(NA)
  }
  if (
    isFALSE(getOption("tinytable_color_name_normalization", default = TRUE))
  ) {
    return(col)
  }

  single_color <- function(k) {
    # Early return for hex colors
    # do this here to allow typst rgb() wrap later
    if (isTRUE(grepl("^#", col))) {
      return(col)
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
        hex_color <- substr(color_val, 2, 7)  # Remove #
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
