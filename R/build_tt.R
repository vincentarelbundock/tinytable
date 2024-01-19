# internal function
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

  # group the table (before style)
  for (l in m$lazy_group) {
    l[["x"]] <- out
    out <- eval(l)
  }

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
