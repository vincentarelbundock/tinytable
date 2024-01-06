bootstrap_setting <- function(x, new, component = "row") {
  att <- attributes(x)    
  out <- strsplit(x, "\n")[[1]]
  if (component == "row") {
    idx <- grep("tinytable columns before this", out)
  }
  out <- c(
    out[1:(idx - 1)],
    new,
    out[idx:length(out)]
  )
  out <- paste(out, collapse = "\n")
  attributes(out) <- att
  class(out) <- class(x)
  return(out)
}



#' @export
spec_row.tinytable_html <- function(x,
                                    i,
                                    halign = NULL,
                                    valign = NULL,
                                    ht = NULL,
                                    bg = NULL,
                                    fg = NULL,
                                    font = NULL,
                                    mode = NULL,
                                    cmd = NULL,
                                    preto = NULL,
                                    appto = NULL,
                                    tabularray = NULL,
                                    ...) {
  assert_integerish(i, lower = 1, null.ok = TRUE)
  if (is.null(i)) i <- seq_len(attr(x, "nrow"))
  out <- x
  for (row in i) {
    if (!is.null(bg)) {
      new <- sprintf("table.rows[%s].style.backgroundColor = '%s';", row, bg)
      out <- bootstrap_setting(out, new, component = "row")
    }
  }
  return(out)
}
