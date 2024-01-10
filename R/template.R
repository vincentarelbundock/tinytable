template_tabularray <- function(theme = "default") {
  assert_choice(theme, c("default", "grid", "void", "striped"))
  if (theme == "default") {
    out <- readLines(system.file("templates/tabularray_default.tex", package = "tinytable"))
  } else if (theme == "striped") {
    out <- readLines(system.file("templates/tabularray_default.tex", package = "tinytable"))
  } else if (theme == "void") {
    out <- readLines(system.file("templates/tabularray_void.tex", package = "tinytable"))
  } else if (theme == "grid") {
    out <- readLines(system.file("templates/tabularray_grid.tex", package = "tinytable"))
  }
  return(out)
}


template_bootstrap <- function(theme = "default") {
  assert_string(theme)
  out <- readLines(system.file("templates/bootstrap.html", package = "tinytable"))

  if (theme == "default") {
    out <- gsub(
      "$tinytable_BOOTSTRAP_CLASS",
      "table",
      out,
      fixed = TRUE)

  } else if (theme == "void") {
    out <- gsub(
      "$tinytable_BOOTSTRAP_CLASS",
      "table table-borderless",
      out,
      fixed = TRUE)

  } else if (theme == "grid") {
    out <- gsub(
      "$tinytable_BOOTSTRAP_CLASS",
      "table table-bordered",
      out,
      fixed = TRUE)

  } else if (theme == "striped") {
    out <- gsub(
      "$tinytable_BOOTSTRAP_CLASS",
      "table table-striped",
      out,
      fixed = TRUE)

  } else {
    out <- gsub(
      "$tinytable_BOOTSTRAP_CLASS",
      theme,
      out,
      fixed = TRUE)
  }

  return(out)
}
