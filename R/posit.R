is_rstudio <- function() {
  identical(Sys.getenv("RSTUDIO"), "1")
}

is_positron <- function() {
  identical(Sys.getenv("POSITRON"), "1")
}

is_rstudio_notebook_context <- function(path, contents = NULL) {
  if (isTRUE(grepl("\\.qmd$|\\.Rmd$", path))) {
    return(TRUE)
  }

  contents <- paste(contents, collapse = "\n")
  if (!nzchar(contents)) {
    return(FALSE)
  }

  has_yaml_header <- grepl("^---\\s*\n", contents)
  has_notebook_format <- grepl("(?m)^\\s*(output|format)\\s*:", contents, perl = TRUE)
  has_chunk <- grepl("(?m)^```\\{[a-zA-Z]", contents, perl = TRUE)

  isTRUE(has_chunk || (has_yaml_header && has_notebook_format))
}

# Best-effort detection of RStudio's chunk output mode. Returns TRUE when chunk
# output is displayed inline (the RStudio default), FALSE when it goes to the
# console. A per-document `editor_options: chunk_output_type:` entry in the YAML
# front matter takes precedence over the global IDE preference.
rstudio_chunk_output_inline <- function(contents = NULL, pref = NULL) {
  yaml <- rstudio_chunk_output_type_yaml(contents)
  if (identical(yaml, "console")) {
    return(FALSE)
  }
  if (identical(yaml, "inline")) {
    return(TRUE)
  }
  if (is.null(pref)) {
    pref <- tryCatch(
      rstudioapi::readRStudioPreference("rmd_chunk_output_inline", TRUE),
      error = function(e) TRUE
    )
  }
  !isFALSE(pref)
}

rstudio_chunk_output_type_yaml <- function(contents = NULL) {
  contents <- unlist(strsplit(paste(contents, collapse = "\n"), "\n"))
  if (length(contents) < 2 || !grepl("^---\\s*$", contents[1])) {
    return(NULL)
  }
  end <- which(grepl("^(---|\\.\\.\\.)\\s*$", contents[-1]))
  if (length(end) == 0) {
    return(NULL)
  }
  header <- contents[2:end[1]]
  m <- regmatches(
    header,
    regexpr("^\\s*chunk_output_type\\s*:\\s*[\"']?(console|inline)", header)
  )
  if (length(m) == 0) {
    return(NULL)
  }
  if (grepl("console", m[1])) "console" else "inline"
}

# Should HTML tables be displayed inline below the chunk in an RStudio notebook?
# Controlled by `options(tinytable_print_rstudio_notebook = "auto"/"inline"/"viewer")`.
is_rstudio_notebook <- function() {
  # inline doesn't work in Positron
  if (!is_rstudio()) {
    return(FALSE)
  }
  mode <- get_option("tinytable_print_rstudio_notebook", default = "auto")
  assert_choice(mode, c("auto", "inline", "viewer"), name = "tinytable_print_rstudio_notebook")
  if (identical(mode, "viewer")) {
    return(FALSE)
  }
  assert_dependency("rstudioapi")
  con <- rstudioapi::getActiveDocumentContext()
  if (!is_rstudio_notebook_context(con[["path"]], con[["contents"]])) {
    return(FALSE)
  }
  if (identical(mode, "inline")) {
    return(TRUE)
  }
  rstudio_chunk_output_inline(con[["contents"]])
}
