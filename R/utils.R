# (pseudo-)unique IDs
get_id <- function(stem = "id") {
  id <- sample(c(0:9, letters), 20, replace = TRUE)
  paste0(stem, paste(id, collapse = ""))
}


meta <- function(x, get, set) {
  meta_attr <- attr(x, "tinytable_meta")

  # no meta yet
  if (is.null(meta_attr)) meta_attr <- list()

  # empty args -> return meta 
  if (missing(get) && missing(set)) return(meta_attr)

  # set new value
  if (!missing(get) && !missing(set)) {
    meta_attr[[get]] <- set
    if (all(c("nrows", "ncols") %in% names(meta_attr))) {
      meta_attr[["ncells"]] <- meta_attr$ncols * meta_attr$nrows
    }
    attr(x, "tinytable_meta") <- meta_attr
    return(x)
  } 

  # return requested value
  return(meta_attr[[get]])
}


ttempdir <- function() {
  d <- tempdir()
  d <- file.path(d, "tinytable")
  # start fresh
  if (dir.exists(d)) unlink(d, recursive = TRUE)
  dir.create(d)
  return(d)
}
