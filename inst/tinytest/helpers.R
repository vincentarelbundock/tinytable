options("tinysnapshot_device" = "svglite")
options("tinysnapshot_tol" = 200)

# common formatting options
options(tinytable_format_bool = function(x) tools::toTitleCase(tolower(x)))
options(tinytable_format_replace = "")


strip_random <- function(x) {
    x <- gsub("tinytable_\\w+\\b", "tinytable", x)
    x <- gsub("styleCell_\\w+\\b", "tinytable", x)
    x <- gsub("spanCell_\\w+\\b", "tinytable", x)
    x <- gsub("insertSpanRow\\w+\\b", "tinytable", x)
    x <- gsub("styleHeaderCell_\\w+\\b", "tinytable", x)
    x
}
options(tinysnapshot_fn_current = strip_random)
options(tinysnapshot_fn_target = strip_random)


is_local <- isTRUE(grepl("(?i)vincent|mbp|mandelbrot", Sys.info()["user"]))

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

print_html <- function(x, output = c("html", "html_portable")) {
    output <- match.arg(output)
    # random table IDs
    set.seed(1024)
    x <- tinytable::save_tt(x, output = output)
    # x <- gsub("tinytable_\\w+\\b", "tinytable", x)
    # x <- gsub("styleCell_\\w+\\b", "tinytable", x)
    # x <- gsub("spanCell_\\w+\\b", "tinytable", x)
    # x <- gsub("insertSpanRow\\w+\\b", "tinytable", x)
    # x <- gsub("styleHeaderCell_\\w+\\b", "tinytable", x)
    class(x) <- c("custom_html_string", "character")
    x
}
