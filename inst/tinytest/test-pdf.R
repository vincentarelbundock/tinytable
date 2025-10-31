source("helpers.R")

if (!is_local) {
  exit_file("Run on Vincent's machine")
}

x <- cbind(mtcars[1, ], mtcars[1, ])
dest_dir <- tempdir()
dest_pdf <- file.path(dest_dir, "out.pdf")
dest_log <- file.path(dest_dir, "somelog.log")

tt(x) |>
  theme_latex(resize_direction = "down") |>
  save_tt(dest_pdf, overwrite = TRUE)

# logfiles are automatically deleted
expect_equal(
  length(grep("\\.log$", list.files(dest_dir))),
  0
)

# logfiles that exist before call to tinytex are left untouched
cat("some content", file = dest_log)

tt(x) |>
  theme_latex(resize_direction = "down") |>
  save_tt(dest_pdf, overwrite = TRUE)

expect_true("somelog.log" %in% list.files(dest_dir))

# Issue #395
x <- mtcars[1:4, 1:5]
fn <- tempfile(fileext = ".pdf")
cap <- "A simple \\texttt{tinytable} example."
not <- "Nullam odio est, ullamcorper scelerisque lectus a, eleifend luctus nisl. Etiam ullamcorper, nibh vel interdum auctor, odio nulla mollis tortor, vel fringilla ante quam quis est."
tt(x, caption = cap, notes = not, width = .5) |>
  style_tt(i = 1:3, j = 1:2, background = "#1ecebf", bold = TRUE) |>
  group_tt(j = list("Halloumi" = 1:2, "Tofu" = 4:5)) |>
  save_tt(fn, overwrite = TRUE)

# Clean slate
suppressWarnings(invisible(file.remove(list.files(dest_dir))))
