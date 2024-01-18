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



# style_tt() stores style calls and we only want to evaluate them at the end because 
# some rows may be added, which changes how the style is applied
build_tt <- function(x) {
  m <- meta(x)

  out <- x

  # format data before drawing the table
  for (l in m$lazy_format) {
    tmp <- out
    class(tmp) <- "data.frame"
    l[["x"]] <- tmp
    out <- eval(l)
  }

  # draw the table
  lazy_tt <- meta(x, "lazy_tt")
  lazy_tt[["x"]] <- out
  out <- eval(lazy_tt)

  # TODO: group the table

  # style the table
  for (l in m$lazy_style) {
    l[["x"]] <- out
    out <- eval(l)
  }

  m <- meta(x)
  m$lazy_style <- list()
  attr(out, "tinytable_meta") <- m

  return(out)
}
