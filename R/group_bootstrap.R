group_bootstrap <- function(x, i = TRUE, italic = TRUE, rule = TRUE, indent = 1) {
  js <- sprintf(
    "window.addEventListener('load', function () { spanCell(%s, %s, '%s') }",
    i, ncol(x), x[i, 1])
  out <- bootstrap_setting(x, js, component = "cells")
  return(out)
}
