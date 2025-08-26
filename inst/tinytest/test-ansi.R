source("helpers.R")
expect_false(TRUE)
exit_file("do this")

pkgload::load_all()
data <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Score = c(95.5, 87.2, 92.8)
)
tt(data, caption = "Three friends.", notes = "Three enemies") |>
  style_tt(i = c(0, 3), color = "orange") |>
  style_tt(i = 1, background = "teal", color = "black", bold = TRUE) |>
  style_tt(i = 2, j = 2, underline = TRUE, color = "red") |>
  style_tt(i = 3, strikeout = TRUE) |>
  group_tt(j = list("Characteristics" = 2:3)) |>
  style_tt(i = "caption", bold = TRUE, color = "red") |>
  style_tt(i = "notes", bold = TRUE, color = "red") |>
  theme_markdown(ansi = TRUE, hline = FALSE)
