source("helpers.R")

expect_true(tinytable:::is_rstudio_notebook_context("/tmp/report.Rmd"))
expect_true(tinytable:::is_rstudio_notebook_context("/tmp/report.qmd"))
expect_false(tinytable:::is_rstudio_notebook_context("/tmp/script.R"))

unsaved_rmd <- c(
  "---",
  "title: Untitled",
  "output: html_document",
  "---",
  "",
  "```{r}",
  "tt(head(mtcars))",
  "```"
)

unsaved_qmd <- c(
  "---",
  "title: Untitled",
  "format: html",
  "---",
  "",
  "```{r}",
  "tt(head(mtcars))",
  "```"
)

unsaved_chunk <- c(
  "```{r}",
  "tt(head(mtcars))",
  "```"
)

expect_true(tinytable:::is_rstudio_notebook_context("", unsaved_rmd))
expect_true(tinytable:::is_rstudio_notebook_context("Untitled3", unsaved_qmd))
expect_true(tinytable:::is_rstudio_notebook_context("Untitled3", unsaved_chunk))
expect_false(tinytable:::is_rstudio_notebook_context("Untitled3", "tt(head(mtcars))"))

# Issue #697: chunk output mode detection
yaml_console <- c(
  "---",
  "title: Untitled",
  "format: html",
  "editor_options:",
  "  chunk_output_type: console",
  "---",
  "",
  "```{r}",
  "tt(head(mtcars))",
  "```"
)
yaml_inline <- sub("console", "inline", yaml_console)
expect_equal(tinytable:::rstudio_chunk_output_type_yaml(yaml_console), "console")
expect_equal(tinytable:::rstudio_chunk_output_type_yaml(yaml_inline), "inline")
expect_null(tinytable:::rstudio_chunk_output_type_yaml(unsaved_qmd))
expect_null(tinytable:::rstudio_chunk_output_type_yaml(unsaved_chunk))
expect_null(tinytable:::rstudio_chunk_output_type_yaml(NULL))

# chunk_output_type outside the YAML header is ignored
body_only <- c(unsaved_qmd, "chunk_output_type: console")
expect_null(tinytable:::rstudio_chunk_output_type_yaml(body_only))

# YAML takes precedence over the global preference
expect_false(tinytable:::rstudio_chunk_output_inline(yaml_console, pref = TRUE))
expect_true(tinytable:::rstudio_chunk_output_inline(yaml_inline, pref = FALSE))
# fall back to the global preference
expect_false(tinytable:::rstudio_chunk_output_inline(unsaved_qmd, pref = FALSE))
expect_true(tinytable:::rstudio_chunk_output_inline(unsaved_qmd, pref = TRUE))

# option overrides; outside RStudio is_rstudio_notebook() is always FALSE
withr_env <- Sys.getenv("RSTUDIO")
Sys.setenv(RSTUDIO = "1")
options(tinytable_print_rstudio_notebook = "viewer")
expect_false(tinytable:::is_rstudio_notebook())
options(tinytable_print_rstudio_notebook = "bad")
expect_error(tinytable:::is_rstudio_notebook(), pattern = "must be one of")
options(tinytable_print_rstudio_notebook = NULL)
Sys.setenv(RSTUDIO = withr_env)
