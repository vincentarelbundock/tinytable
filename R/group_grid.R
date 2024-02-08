group_grid <- function(x, i = NULL, j = NULL, ...) {
  out <- x

  if (!is.null(i)) {
    out <- group_grid_row(out, i)
  }

  if (!is.null(j)) {
    out <- group_grid_col(out, j)
  }

  return(out)
}


group_grid_col <- function(x, j, ...) {
  m <- meta(x)
  header <- empty_cells(j)
  cw <- meta(x, "col_widths")
  cw <- sapply(header, function(k) sum(cw[k]) + length(cw[k]) - 1)
  txt <- t(matrix(names(cw)))
  out <- tt_grid(txt, cw)
  out <- strsplit(out, split = "\\n")[[1]]
  out <- out[out != "\\n"]
  out <- out[!out %in% c("\\n", "")]
  out <- out[2]
  x <- strsplit(x, split = "\\n")[[1]]
  x <- x[!x %in% c("\\n", "")]
  x <- c(x[1], out, x)

  # missing cell at the end
  nc <- nchar(x)
  idx <- nchar(x) < max(nc)
  x[idx] <- paste0(x[idx], strrep(" ", max(nc) - nchar(x[idx]) - 1), "|")

  out <- paste(x, collapse = "\n")
  attr(out, "tinytable_meta") <- m
  class(out) <- class(x)
  return(out)
}



group_grid_row <- function(x, i, ...) {
  out <- x
  out <- strsplit(x, split = "\\n")[[1]]
  out <- out[out != ""]
  # header
  body_min <- utils::head(grep("^\\+==", out), 1) + 1
  # no header
  if (is.na(body_min) || length(body_min) == 0) {
    body_min <- utils::head(grep("^\\+--", out), 1) + 1
  }
  body_max <- utils::tail(grep("^\\+--", out), 1) - 1
  body <- body_min:body_max
  top <- out[1:(min(body) - 1)]
  mid <- out[min(body):max(body)]
  bot <- out[(max(body) + 1):length(out)]

  cw <- meta(x, "col_widths")
  cw <- sum(cw) + length(cw) - 1
  for (idx in rev(seq_along(i))) {
    tmp <- as.character(tt_grid(matrix(names(i)[idx]), col_widths = cw))
    tmp <- strsplit(tmp, split = "\\n")[[1]]
    tmp <- tmp[tmp != ""][2]
    lo <- i[idx] - 1
    if (lo > 0) {
      mid <- c(mid[1:lo], tmp, mid[i[idx]:length(mid)]) 
    } else {
      mid <- c(tmp, mid[i[idx]:length(mid)]) 
    }
  }

  out <- c(top, mid, bot)
  out <- paste(out, collapse = "\n")

  attr(out, "tinytable_meta") <- meta(x)
  class(out) <- class(x)
  return(out)
}

