typst_escape_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x
}

typst_image_mime <- function(images) {
  ext <- tolower(tools::file_ext(images))

  if (any(ext == "")) {
    stop("Empty image extensions are not allowed", call. = FALSE)
  }

  ext <- ifelse(ext == "jpg", "jpeg", ext)
  ext <- ifelse(ext == "svg", "svg+xml", ext)
  paste0("image/", ext)
}

encode_typst <- function(images, height, width_plot, height_plot) {
  assert_dependency("base64enc")

  if (any(grepl("^https?://", trimws(images)))) {
    stop("Portable Typst images require local image files.", call. = FALSE)
  }

  width_plot <- format_markup_num(width_plot)
  height_plot <- format_markup_num(height_plot)
  height <- format_markup_num(height)
  mime <- typst_image_mime(images)
  encoded <- sapply(images, base64enc::base64encode)
  encoded <- gsub("\\s+", "", encoded)

  svg <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='%s' height='%s' viewBox='0 0 %s %s'><image href='data:%s;base64,%s' width='%s' height='%s' preserveAspectRatio='xMidYMid meet'/></svg>",
    width_plot,
    height_plot,
    width_plot,
    height_plot,
    mime,
    encoded,
    width_plot,
    height_plot
  )
  svg <- typst_escape_string(svg)

  base::sprintf('#image(bytes("%s"), format: "svg", height: %sem)', svg, height)
}
