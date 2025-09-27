options("tinysnapshot_device" = "svglite")
options("tinysnapshot_tol" = 300)
options("tinysnapshot_mode" = "sidebyside")
options("width" = 200) # for diff printout readability
options("tinytable_print_output" = NULL)
options("tinytable_html_engine" = NULL)

# common formatting options
options(tinytable_format_bool = function(x) tools::toTitleCase(tolower(x)))
options(tinytable_format_replace = "")

strip_random_sequential <- function(x, stem = "tinytable_css_") {
  pattern <- paste0(stem, "\\w+\\b")
  match_list <- lapply(x, function(s) {
    regmatches(s, gregexpr(pattern, s, perl = TRUE))[[1]]
  })
  all_matches <- unlist(match_list)
  unique_matches <- unique(all_matches)
  new_ids <- sprintf("tinytable_css_%02d", seq_along(unique_matches))
  replaced <- x
  for (i in seq_along(unique_matches)) {
    replaced <- gsub(unique_matches[i], new_ids[i], replaced, perl = TRUE)
  }
  replaced
}
strip_random <- function(x) {
  for (stem in c(
    "insertSpanRow",
    "spanCell_",
    "styleCell_",
    "styleHeaderCell_",
    "tableFns_",
    "tinytable_(?!css)",
    "tinytable_css_",
    "tinytable/"
  )) {
    x <- strip_random_sequential(x, stem)
  }
  x
}
options(tinysnapshot_fn_current = strip_random)
options(tinysnapshot_fn_target = strip_random)

is_local <- isTRUE(grepl("(?i)vincent|mbp|mandelbrot", Sys.info()["user"]))

# libraries
requiet <- function(package) {
  void <- capture.output(
    pkg_available <- tryCatch(
      suppressPackageStartupMessages(
        suppressWarnings(
          suppressMessages(
            tryCatch(
              isTRUE(
                require(package, warn.conflicts = FALSE, character.only = TRUE)
              ),
              error = function(e) FALSE
            )
          )
        )
      )
    )
  )
  return(pkg_available)
}

requiet("tinytable")
requiet("tinytest")
requiet("tinysnapshot")

print.custom_html_string <- function(x, ...) {
  cat(x, "\n", sep = "", ...)
  invisible(x)
}

print_html <- function(x, output = c("html")) {
  output <- match.arg(output)
  x <- save_tt(x, output)
  class(x) <- c("custom_html_string", class(x))
  x
}

format_extensions <- list(
  markdown = "md",
  latex = "tex",
  typst = "typ",
  html = "html"
)

# Helper function to prepare tables for testing across multiple formats
expect_table <- function(tab, formats = names(format_extensions)) {
  result <- list()

  for (format in formats) {
    # Create a copy and set the output format
    table_copy <- tab
    table_copy@output <- format

    # Create the table output
    if (format == "html") {
      table_output <- print_html(table_copy)
    } else {
      table_output <- table_copy
    }

    # Store the table output
    result[[format]] <- table_output
  }

  return(result)
}
