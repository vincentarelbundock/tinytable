theme_drop_default <- function(x) {
  if (length(x@theme) > 0 && identical(x@theme[[1]], "default")) {
    x@theme[[1]] <- NULL
  }
  return(x)
}
