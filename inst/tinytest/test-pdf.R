source("helpers.R")

x <- cbind(mtcars[1, ], mtcars[1, ])
dest_dir <- tempdir()
dest_pdf <- file.path(dest_dir, "out.pdf")
dest_log <- file.path(dest_dir, "somelog.log")

expect_warning(
    tt(x) |>
        theme_tt("resize") |>
        save_tt(dest_pdf),
    "Table width is too small"
)

# logfiles are automatically deleted
expect_equal(
    length(grep("\\.log$", list.files(dest_dir))),
    0
)

# logfiles that exist before call to tinytex are left untouched
cat("some content", file = dest_log)

expect_warning(
    tt(x) |>
        theme_tt("resize") |>
        save_tt(dest_pdf, overwrite = TRUE),
    "Table width is too small"
)

expect_true("somelog.log" %in% list.files(dest_dir))

# Clean slate
suppressWarnings(invisible(file.remove(list.files(dest_dir))))
