# Make colors more uniform: R definition -> LaTeX definition -> default
rcolors <- function(col, format = "hex") {
  if (length(col) == 1 && is.na(col)) {
    return(NA)
  }
  if (isFALSE(getOption("tinytable_color_name_normalization", default = TRUE))) {
    return(col)
  }

  fun <- function(k) {
    # Early return for hex colors
    # do this here to allow typst rgb() wrap later
    if (length(col) == 1 && grepl("^#", col)) {
      return(col)
    }

    # R colors
    out <- try(col2rgb(k), silent = TRUE)

    if (!inherits(out, "try-error")) {
      # Convert RGB values to hex format
      out <- rgb(out[1], out[2], out[3], maxColorValue = 255)
    } else if (k %in% latex_colors$name) {
      # LaTeX colors
      hex_val <- latex_colors$rgb[latex_colors$name == k]
      out <- paste0("#", hex_val)
    } else {
      # If still no match, return the original value
      out <- k
    }
    return(out)
  }
  result <- sapply(col, fun)

  # Format for Typst if needed
  if (format == "typst") {
    result <- ifelse(is.character(result) & grepl("^#", result), sprintf('rgb("%s")', result), result)
  }

  return(result)
}
