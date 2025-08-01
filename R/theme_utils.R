theme_drop_default <- function(x) {
  if (identical(x@theme[[1]], "default")) {
    x@theme[[1]] <- NULL
  }
  return(x)
}
