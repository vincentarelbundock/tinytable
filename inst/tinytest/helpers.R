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

requiet("tinytable")
requiet("tinytest")
requiet("tinysnapshot")

print.custom_html_string <- function(x, ...) {
    cat(x, "\n", sep = "", ...)
    invisible(x)
}

print_html <- function(x) {
  # random table IDs
  set.seed(1024)
  x <- tinytable::save_tt(x, output = "html")
  # x <- gsub("tinytable_\\w+\\b", "tinytable", x)
  # x <- gsub("styleCell_\\w+\\b", "tinytable", x)
  # x <- gsub("spanCell_\\w+\\b", "tinytable", x)
  # x <- gsub("insertSpanRow\\w+\\b", "tinytable", x)
  # x <- gsub("styleHeaderCell_\\w+\\b", "tinytable", x)
  class(x) <- c("custom_html_string", "character")
  x
}
