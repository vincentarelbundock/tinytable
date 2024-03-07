setMethod(
  f = "finalize",
  signature = "tinytable_tabularray",
  definition = function(x, ...) {
    for (fn in x@lazy_finalize) {
      x <- fn(x)
    }
    return(x)
  }
)