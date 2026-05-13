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
