grid_line <- function(width_cols, char = "-") {
  line_sep <- lapply(width_cols, function(k) strrep(char, k))
  line_sep <- paste(line_sep, collapse = "+")
  line_sep <- paste0("+", line_sep, "+")
  return(line_sep)
}


tt_grid <- function(x, width_cols = NULL, ...) {

  is_matrix <- is.matrix(x)
  if (is_matrix) {
    tab <- x
  } else {
    tab <- x@table_dataframe
  }

  if (is.null(width_cols)) {
      width_cols <- x@width_cols
  }

  if (!is.null(names(tab))) {
    tab <- as.matrix(tab)
    tab <- rbind(colnames(x), tab)
    header <- TRUE
  } else {
    tab <- as.matrix(tab)
    header <- FALSE
  }

  # pad for readability
  padded <- sprintf(" %s ", tab)
  tab <- matrix(padded, ncol = ncol(tab))

  if (is.null(width_cols) || length(width_cols) == 0) {
    for (j in 1:ncol(tab)) {
      width_cols[j] <- max(nchar(tab[, j]))
    }
  }

  # groups are longer than col-widths
  if (inherits(x, "tinytable")) {
    for (g in x@lazy_group) {
      for (idx in seq_along(g$j)) {
        g_len <- nchar(names(g$j)[idx]) + 2
        c_len <- sum(width_cols[g$j[[idx]]])
        if (g_len > c_len) {
          width_cols[g$j[[idx]]] <- width_cols[g$j[[idx]]] + ceiling((g_len - c_len) / length(g$j[[idx]]))
        }
      }
    }
  }

  for (j in 1:ncol(x)) {
    nc <- nchar(tab[, j])
    pad <- width_cols[j] - nc
    pad <- sapply(pad, function(k) strrep(" ", k))
    tab[, j] <- paste0(tab[, j], pad)
  }


  rule_head <- grid_line(width_cols, "=")
  rule_line <- grid_line(width_cols, "-")

  body <- apply(tab, 1, paste, collapse = "|")
  body <- paste0("|", body, "|")
  if (header) {
    tab <- c("\n", rule_line, body[1], rule_head, body[2:length(body)], rule_line, "\n")
  } else {
    tab <- c("\n", rule_line, body, rule_line, "\n")
  }

  out <- paste(tab, collapse = "\n")

  if (is_matrix) return(out)

  # rebuild output
  x@width_cols <- width_cols
  x@table_string <- out

  # output
  return(x)
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
  rule_line <- grid_line(x@width_cols, "-")
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
