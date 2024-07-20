source("helpers.R")

x <- format_tt(glue::glue("hi"), escape = "latex")
expect_inherits(x, "character")
expect_true(length(x) == 1)
