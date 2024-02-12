options("tinysnapshot_device" = "svglite")
options("tinysnapshot_tol" = 200)

# libraries
requiet <- function(package) {
    void <- capture.output(
    pkg_available <- tryCatch(suppressPackageStartupMessages(suppressWarnings(suppressMessages(tryCatch(
        isTRUE(require(package, warn.conflicts = FALSE, character.only = TRUE)),
        error = function(e) FALSE
    ))))))
    return(pkg_available)
}

requiet("tinytest")
requiet("tinysnapshot")



clean_html <- function(x) {
  x <- gsub("tinytable_\\w+\\b", "tinytable", x)
  x <- gsub("styleCell_\\w+\\b", "tinytable", x)
  x <- gsub("insertSpanRow\\w+\\b", "tinytable", x)
  x <- gsub("styleHeaderCell_\\w+\\b", "tinytable", x)
  x
}
