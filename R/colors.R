# Make colors more uniform: R definition -> LaTeX definition -> default
standardize_colors <- function(col, format = "hex") {
  if (length(col) == 1 && is.na(col)) {
    return(NA)
  }
  if (isFALSE(getOption("tinytable_color_name_normalization", default = TRUE))) {
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
    result <- ifelse(is.character(result) & grepl("^#", result), sprintf('rgb("%s")', result), result)
  }

  return(result)
}
