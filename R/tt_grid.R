
grid_line <- function(col_widths, char = "-") {
  line_sep <- lapply(col_widths, function(k) strrep(char, k))
  line_sep <- paste(line_sep, collapse = "+")
  line_sep <- paste0("+", line_sep, "+")
  return(line_sep)
}

tt_grid <- function(x, col_widths = NULL) {

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
  attr(out, "col_widths") <- col_widths
  out <- meta(out, "output", "grid")

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


group_grid <- function(x, j) {
  header <- empty_cells(j)
  cw <- attr(x, "col_widths")
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
  return(out)
}


df <- data.frame(
  Fruit = c("Bananas", "Oranges"),
  Price = c("$1.34", "$2.10"),
  Features = c(2, 4.3),
  Color = c("Yellow", "Orange"),
  Smell = c("Bad", "Good")
)


pkgload::load_all()
x <- tt_grid(df)
j = list("foo" = 2:3, "bar" = 4:5)
z = group_grid(x, j)
cat(z)
