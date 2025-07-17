grid_line <- function(width_cols, char = "-") {
  line_sep <- lapply(width_cols, function(k) strrep(char, k))
  line_sep <- paste(line_sep, collapse = "+")
  line_sep <- paste0("+", line_sep, "+")
  return(line_sep)
}

tt_eval_grid <- function(x, width_cols = NULL, ...) {
  is_matrix <- is.matrix(x)
  if (is_matrix) {
    tab <- x
  } else {
    tab <- x@table_dataframe
  }

  if (is.null(width_cols)) {
    width_cols <- x@width_cols
  }

  tthead <- inherits(x, "tinytable") && isTRUE(x@nhead > 0)
  if (length(colnames(x)) != 0 || tthead) {
    tab <- as.matrix(tab)
    tab <- base::rbind(colnames(x), tab)
    header <- TRUE
  } else {
    tab <- as.matrix(tab)
    header <- FALSE
  }

  # pad for readability
  padded <- sprintf(" %s ", tab)
  tab <- matrix(padded, ncol = ncol(tab))

  if (is.null(width_cols) || length(width_cols) == 0) {
    for (j in seq_len(ncol(tab))) {
      if (isTRUE(check_dependency("fansi"))) {
        width_cols[j] <- max(nchar(as.character(fansi::strip_ctl(tab[, j]))))
      } else {
        width_cols[j] <- max(nchar(tab[, j]))
      }
    }
  }

  # groups are longer than col-widths
  if (inherits(x, "tinytable")) {
    for (g in x@lazy_group) {
      # Extract arguments from the lazy evaluation call
      call_args <- as.list(g)[-1] # Remove function name

      # Handle column groups
      if (!is.null(call_args$j)) {
        j_groups <- eval(call_args$j)
        for (idx in seq_along(j_groups)) {
          g_len <- nchar(names(j_groups)[idx]) + 2
          c_len <- sum(width_cols[j_groups[[idx]]])
          if (g_len > c_len) {
            width_cols[j_groups[[idx]]] <- width_cols[j_groups[[idx]]] +
              ceiling((g_len - c_len) / length(j_groups[[idx]]))
          }
        }
      }

      # Handle row groups (they span entire table width)
      if (!is.null(call_args$i)) {
        i_groups <- eval(call_args$i)
        for (idx in seq_along(i_groups)) {
          g_len <- nchar(names(i_groups)[idx]) + 2
          # Total table width including separators
          c_len <- sum(width_cols) + length(width_cols) - 1
          if (g_len > c_len) {
            # Distribute extra width across all columns
            extra_width <- ceiling((g_len - c_len) / length(width_cols))
            width_cols <- width_cols + extra_width
          }
        }
      }
    }
  }

  for (j in seq_len(ncol(x))) {
    if (isTRUE(check_dependency("fansi"))) {
      nc <- nchar(fansi::strip_ctl(tab[, j]))
    } else {
      nc <- nchar(tab[, j])
    }
    pad <- width_cols[j] - nc
    pad <- sapply(pad, function(k) strrep(" ", max(0, k)))
    tab[, j] <- paste0(tab[, j], pad)
  }

  rule_head <- grid_line(width_cols, "=")
  rule_line <- grid_line(width_cols, "-")

  body <- apply(tab, 1, paste, collapse = "|")
  body <- paste0("|", body, "|")
  if (header) {
    tab <- c(
      "\n",
      rule_line,
      body[1],
      rule_head,
      body[2:length(body)],
      rule_line,
      "\n"
    )
  } else {
    tab <- c("\n", rule_line, body, rule_line, "\n")
  }

  out <- paste(tab, collapse = "\n")

  if (is_matrix) {
    return(out)
  }

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
  out <- x@table_string
  lines <- strsplit(out, split = "\\n")[[1]]
  if (length(lines) > 1) {
    for (idlines in length(lines):2) {
      if (
        !startsWith(lines[idlines - 1], "+") &&
          !startsWith(lines[idlines], "+") &&
          lines[idlines] != ""
      ) {
        lines <- c(
          lines[1:(idlines - 1)],
          rule_line,
          lines[idlines:length(lines)]
        )
      }
    }
  }
  x@table_string <- paste(lines, collapse = "\n")
  return(x)
}

setMethod(
  f = "tt_eval",
  signature = "tinytable_grid",
  definition = tt_eval_grid
)

setMethod(
  f = "tt_eval",
  signature = "matrix",
  definition = tt_eval_grid
)

setMethod(
  f = "tt_eval",
  signature = "tinytable_dataframe",
  definition = tt_eval_grid
)
