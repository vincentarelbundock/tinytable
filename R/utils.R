# (pseudo-)unique IDs
get_id <- function(stem = "id") {
  id <- sample(c(0:9, letters), 20, replace = TRUE)
  paste0(stem, paste(id, collapse = ""))
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


lines_drop <- function(old, regex, position = "equal") {
  assert_choice(position, c("equal", "before", "after"))
  lines <- strsplit(old, "\n")[[1]]
  idx <- grep(regex, lines)
  if (length(idx) > 1) {
    stop("The `regex` supplied `lines_drop()` did not match a unique line.", call. = FALSE)
  }
  if (!anyNA(idx)) {
    if (position == "equal") {
      lines <- lines[!seq_along(lines) %in% idx]
    } else if (position == "before") {
      lines <- lines[idx:length(lines)]
    } else if (position == "after") {
      lines <- lines[1:idx]
    }
  }
  out <- paste(lines, collapse = "\n")
  return(out)
}


lines_insert <- function(old, new, regex, position = "before") {
    lines <- strsplit(old, "\n")[[1]]
    idx <- grep(regex, lines)
    if (length(idx) != 1 || anyNA(idx)) {
          stop("The `regex` supplied `lines_drop()` did not match a unique line.", call. = FALSE)
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