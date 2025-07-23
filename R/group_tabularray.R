#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval_j",
  signature = "tinytable_tabularray",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
    # Only handle column grouping - row insertions now use matrix insertion
    x <- group_tabularray_col(x, j, ...)
    return(x)
  }
)

group_tabularray_col <- function(x, j, ihead, ...) {
  if (is.null(j)) {
    return(x)
  }

  out <- strsplit(x@table_string, split = "\\n")[[1]]

  header <- rep("", ncol(x))
  for (idx in seq_along(j)) {
    header[min(j[[idx]])] <- names(j)[idx]
  }
  header <- paste(header, collapse = " & ")

  # \toprule -> \midrule
  midr <- sapply(
    j,
    function(x) sprintf("\\cmidrule[lr]{%s-%s}", min(x), max(x))
  )
  header <- paste(header, "\\\\", paste(midr, collapse = ""))

  idx <- max(
    c(
      grep("% tabularray inner close", out),
      grep("\\toprule", out, fixed = TRUE)
    )
  )

  out <- c(
    out[1:idx],
    # empty lines can break latex
    trimws(header),
    out[(idx + 1):length(out)]
  )
  out <- paste(out, collapse = "\n")

  # rebuild including meta before style_tt
  x@table_string <- out

  for (k in seq_along(j)) {
    z <- min(j[[k]])
    cs <- max(j[[k]]) - min(j[[k]]) + 1
    if (cs == 1) {
      cs <- NULL
    }
    args <- list(
      tt_build_now = TRUE,
      x = x,
      i = ihead,
      j = z,
      align = "c",
      colspan = cs
    )
    x <- do.call(style_tt, args)
  }

  return(x)
}


