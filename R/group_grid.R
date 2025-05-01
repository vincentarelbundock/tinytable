#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_grid",
  definition = function(x, i = NULL, j = NULL, ...) {
    # add here because this is where lazy gets executed
    x <- group_grid_row(x, i)
    x <- group_grid_col(x, j)
    return(x)
  }
)

group_grid_col <- function(x, j, ...) {
  if (is.null(j)) {
    return(x)
  }
  tab <- x@table_string
  header <- empty_cells(j)
  cw <- x@width_cols
  cw <- sapply(header, function(k) sum(cw[k]) + length(cw[k]) - 1)
  header <- t(matrix(names(cw)))
  header <- tt_eval(header, cw)
  header <- strsplit(header, split = "\\n")[[1]]
  header <- header[header != "\\n"]
  header <- header[!header %in% c("\\n", "")]
  header <- header[2]
  z <- strsplit(tab, split = "\\n")[[1]]
  z <- z[!z %in% c("\\n", "")]
  z <- c(z[1], header, z)

  # missing cell at the end
  nc <- nchar(z)
  idx <- nchar(z) < max(nc)
  z[idx] <- paste0(z[idx], strrep(" ", max(nc) - nchar(z[idx]) - 1), "|")

  tab <- paste(z, collapse = "\n")

  x@table_string <- tab

  return(x)
}

group_grid_row <- function(x, i, ...) {
  if (is.null(i)) {
    return(x)
  }
  tab <- x@table_string
  tab <- strsplit(tab, split = "\\n")[[1]]
  tab <- tab[tab != ""]
  # header
  body_min <- utils::head(grep("^\\+==", tab), 1) + 1
  # no header
  if (is.na(body_min) || length(body_min) == 0) {
    body_min <- utils::head(grep("^\\+--", tab), 1) + 1
  }
  body_max <- utils::tail(grep("^\\+--", tab), 1) - 1
  body <- body_min:body_max
  top <- tab[1:(min(body) - 1)]
  mid <- tab[min(body):max(body)]
  bot <- tab[(max(body) + 1):length(tab)]

  cw <- x@width_cols
  cw <- sum(cw) + length(cw) - 1
  for (idx in rev(seq_along(i))) {
    tmp <- matrix(names(i)[idx])
    tmp <- as.character(tt_eval(tmp, width_cols = cw))
    tmp <- strsplit(tmp, split = "\\n")[[1]]
    tmp <- tmp[tmp != ""][2]
    lo <- i[idx] - 1
    mid <- append(mid, tmp, after = lo)
  }

  tab <- c(top, mid, bot)
  tab <- paste(tab, collapse = "\n")

  x@table_string <- tab

  return(x)
}
