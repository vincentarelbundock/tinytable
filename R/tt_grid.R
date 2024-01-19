grid_line <- function(col_widths, char = "-") {
  line_sep <- lapply(col_widths, function(k) strrep(char, k))
  line_sep <- paste(line_sep, collapse = "+")
  line_sep <- paste0("+", line_sep, "+")
  return(line_sep)
}


tt_grid <- function(x, col_widths = NULL, ...) {

  m <- meta(x)

  if (is.null(col_widths)) {
    col_widths <- m$col_widths
  }

  tab <- as.matrix(x)
  if (!is.null(names(x))) {
    header <- TRUE
    tab <- rbind(colnames(x), tab)
  } else {
    header <- FALSE
  }

  # pad for readability
  padded <- sprintf(" %s ", tab)
  tab <- matrix(padded, ncol = ncol(tab))

  col_widths_auto <- NULL
  for (j in 1:ncol(x)) {
    if (is.null(col_widths)) {
      tab[, j] <- format(tab[, j])
      col_widths_auto[j] <- nchar(tab[1, j])
    } else {
      tab[, j] <- format(tab[, j], width = col_widths[j])
    }
  }

  if (is.null(col_widths)) col_widths <- col_widths_auto

  rule_head <- grid_line(col_widths, "=")
  rule_line <- grid_line(col_widths, "-")

  body <- apply(tab, 1, paste, collapse = "|")
  body <- paste0("|", body, "|")
  if (header) {
    tab <- c("\n", rule_line, body[1], rule_head, body[2:length(body)], rule_line, "\n")
  } else {
    tab <- c("\n", rule_line, body, rule_line, "\n")
  }

  out <- paste(tab, collapse = "\n")

  # rebuild output
  attr(out, "tinytable_meta") <- m
  out <- meta(out, "col_widths", col_widths)
  out <- meta(out, "output", "grid")
  class(out) <- c("tinytable", "knitr_kable")

  # output
  return(out)
}


empty_cells <- function(lst) {
  # Find the largest number in the list
  max_num <- max(unlist(lst))

  # Create the full range from 1 to the largest number
  full_range <- 1:max_num

  # Find missing numbers (holes)
  missing_nums <- setdiff(full_range, unlist(lst))

  # Create new elements for missing numbers
  new_elements <- split(missing_nums, cumsum(c(1, diff(missing_nums) != 1)))

  # Name the new elements with empty strings and merge with original list
  names(new_elements) <- rep(" ", length(new_elements))
  filled_list <- c(lst, new_elements)

  # Sort the list by the minimum number in each element
  filled_list[order(sapply(filled_list, min))]
}


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
  # columns
  header <- empty_cells(j)
  cw <- meta(x, "col_widths")
  cw <- sapply(header, function(k) sum(cw[k]) + length(cw[k]) - 1)
  txt <- t(matrix(names(cw)))
  out <- tt_grid(txt, cw)
  out <- strsplit(out, split = "\\n")[[1]]
  out <- out[out != "\\n"]
  out <- out[!out %in% c("\\n", "")]
  x <- strsplit(x, split = "\\n")[[1]]
  x <- x[!x %in% c("\\n", "")]
  out <- out[1:(length(out) - 1)]
  out <- paste(c(out, x), collapse = "\n")
  attr(out, "tinytable_meta") <- m
  class(out) <- class(x)
  return(out)
}



group_grid_row <- function(x, i, ...) {
  out <- x
  out <- strsplit(x, split = "\\n")[[1]]
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
    tmp <- trimws(as.character(tt_grid(matrix(names(i)[idx]), col_widths = cw)))
    tmp <- strsplit(tmp, split = "\\n")[[1]][2]
    mid <- c(mid[1:(i[idx] - 1)], tmp, mid[i[idx]:length(body)]) 
  }

  out <- c(top, mid, bot)
  out <- paste(out, collapse = "\n")

  attr(out, "tinytable_meta") <- meta(x)
  class(out) <- class(x)
  return(out)
}



# insert horizontal rules everywhere (important for word)
grid_hlines <- function(x) {
  rule_line <- grid_line(meta(x, "col_widths"), "-")
  lines <- strsplit(x, split = "\\n")[[1]]
  if (length(lines) > 1) {
    for (idlines in length(lines):2) {
      if (!startsWith(lines[idlines - 1], "+") && !startsWith(lines[idlines], "+") && lines[idlines] != "") {
        lines <- c(lines[1:(idlines - 1)], rule_line, lines[idlines:length(lines)])
      }
    }
  }
  out <- paste(lines, collapse = "\n")
  return(out)
}
