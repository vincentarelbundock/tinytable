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


# remove consecutive empty lines
lines_drop_empty <- function(x) {
  lines <- strsplit(x, "\n")[[1]]
  tmp = rle(lines)
  idx <- which(tmp$lengths == 1)
  tmp$values <- tmp$values[idx]
  tmp$lengths <- tmp$length[idx]
  lines <- inverse.rle(tmp) 
  x <- paste0(lines, collapse = "\n")
  return(x)
}