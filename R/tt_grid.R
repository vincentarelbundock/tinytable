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

  if (is.null(col_widths)) {
    for (j in 1:ncol(x)) {
      col_widths[j] <- max(nchar(tab[, j]))
    }
  }

  # groups are longer than col-widths
  for (g in meta(x, "lazy_group")) {
    for (idx in seq_along(g$j)) {
      g_len <- nchar(names(g$j)[idx]) + 2
      c_len <- sum(col_widths[g$j[[idx]]])
      if (g_len > c_len) {
        col_widths[g$j[[idx]]] <- col_widths[g$j[[idx]]] + ceiling((g_len - c_len) / length(g$j[[idx]]))
      }
    }
  }

  for (j in 1:ncol(x)) {
    nc <- nchar(tab[, j])
    tab[, j] <- paste0(tab[, j], strrep(" ", max(c(0, col_widths[j] - nc))))
  }

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

  if (length(missing_nums) == 0) {
    return(lst)
  }

  # Create new elements for missing numbers
  new_elements <- split(missing_nums, cumsum(c(1, diff(missing_nums) != 1)))

  # Name the new elements with empty strings and merge with original list
  names(new_elements) <- rep(" ", length(new_elements))
  filled_list <- c(lst, new_elements)

  # Sort the list by the minimum number in each element
  filled_list[order(sapply(filled_list, min))]
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
