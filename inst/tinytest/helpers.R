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

