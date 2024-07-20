# (pseudo-)unique IDs
get_id <- function(stem = "id") {
  id <- sample(c(0:9, letters), 20, replace = TRUE)
  paste0(stem, paste(id, collapse = ""))
}

# getOption with deprecation warnings
get_option <- function(x, default = NULL) {
  deprecated = c(
    # old = new
    "tinytable_grid_hlines" = "tinytable_markdown_hlines"
  )
  if (x %in% names(deprecated)) {
    x_new = deprecated[x]
    warning(
      sprintf("Option `%s` is deperacated. Use `%s` instead.", x, x_new)
    )
    x = x_new
  }
  getOption(x, default = default)
}

ttempdir <- function() {
  d <- tempdir()
  d <- file.path(d, "tinytable")
  # start fresh
  if (dir.exists(d)) unlink(d, recursive = TRUE)
  dir.create(d)
  return(d)
}


lines_drop_consecutive_empty <- function(x) {
  lines <- strsplit(x, "\n")[[1]]
  tmp = rle(lines)
  tmp$lengths[trimws(tmp$values) == ""] <- 1
  lines <- inverse.rle(tmp) 
  x <- paste0(lines, collapse = "\n")
  return(x)
}


lines_drop <- function(old, regex, position = "equal", fixed = FALSE, unique = TRUE) {
  assert_choice(position, c("equal", "before", "after", "all"))
  lines <- strsplit(old, "\n")[[1]]
  idx <- grep(regex, lines, fixed = fixed)
  if (isTRUE(unique) && length(idx) > 1 && position != "all") {
    stop("The `regex` supplied `lines_drop()` did not match a unique line.", call. = FALSE)
  }
  if (!anyNA(idx)) {
    if (position == "equal") {
      lines <- lines[!seq_along(lines) %in% idx]
    } else if (position == "before") {
      lines <- lines[idx:length(lines)]
    } else if (position == "after") {
      lines <- lines[1:idx]
    } else if (position == "all") {
      lines <- lines[!seq_along(lines) %in% idx]
    }
  }
  out <- paste(lines, collapse = "\n")
  return(out)
}


lines_drop_between <- function(text, regex_start, regex_end, fixed = FALSE) {
  lines <- strsplit(text, "\n")[[1]]
  idx_start <- grep(regex_start, lines, fixed = fixed)
  idx_end <- grep(regex_end, lines, fixed = fixed)
  if (length(idx_start) != 1) {
    stop("The `regex_start` did not match a unique line.", call. = FALSE)
  }
  if (length(idx_end) != 1) {
    stop("The `regex_end` did not match a unique line.", call. = FALSE)
  }
  if (idx_start >= idx_end) {
    stop("`regex_start` matches a line after `regex_end`.", call. = FALSE)
  }
  lines_to_keep <- c(1:(idx_start-1), (idx_end+1):length(lines))
  output <- lines[lines_to_keep]
  out <- paste(output, collapse = "\n")
  return(out)
}


lines_insert <- function(old, new, regex, position = "before") {
    lines <- strsplit(old, "\n")[[1]]
    idx <- grep(regex, lines)
    if (length(idx) != 1 || anyNA(idx)) {
          stop("The `regex` supplied `lines_insert()` did not match a unique line.", call. = FALSE)
    }
    if (position == "before") {
        top <- lines[1:(idx - 1)]
        bot <- lines[idx:length(lines)]
    } else if (position == "after") {
        top <- lines[1:idx]
        bot <- lines[(idx + 1):length(lines)]
    }
    lines <- c(top, new, bot)
    out <- paste(lines, collapse = "\n")
    return(out)
}
