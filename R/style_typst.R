#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
style_typst <- function(x,
                        i = NULL,
                        j = NULL,
                        bold = FALSE,
                        italic = FALSE,
                        monospace = FALSE,
                        underline = FALSE,
                        strikeout = FALSE,
                        color = NULL,
                        background = NULL,
                        fontsize = NULL,
                        width = NULL,
                        align = NULL,
                        line = NULL,
                        line_color = "black",
                        line_width = .1,
                        colspan = NULL,
                        indent = 0,
                        ...) {


  if (meta(x, "output") != "typst") return(x)

  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  # only columns means we also want to style headers 
  if (is.null(i) && !is.null(j)) {
    ival <- c(-1 * rev(seq_len(meta(x)$nhead) - 1), ival)
  }

  if (!is.null(background)) {
    tmp <- sprintf("
    // background
    let i = (%s,);
    let j = (%s,);
    if (i.contains(cell.y) and j.contains(cell.x)) { cell.fill = %s };
    ",
    paste(ival, collapse = ","),
    paste(jval, collapse = ","),
    background)
    typst_insert(out, tmp, type = "style")
  }

}