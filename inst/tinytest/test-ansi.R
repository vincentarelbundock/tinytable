source("helpers.R")
using("tinysnapshot")

options(tinytable_print_output = "markdown")
data <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Score = c(95.5, 87.2, 92.8)
)
tab <- tt(data, caption = "Three friends.") |>
  style_tt(i = c(0, 3), color = "orange") |>
  style_tt(i = 1, background = "teal", color = "black", bold = TRUE) |>
  style_tt(i = 2, j = 2, underline = TRUE, color = "red") |>
  style_tt(i = 3, strikeout = TRUE) |>
  group_tt(j = list("Characteristics" = 2:3)) |>
  style_tt(i = "caption", bold = TRUE, color = "red") |>
  style_tt(i = "notes", bold = TRUE, color = "red") |>
  theme_markdown(ansi = TRUE, hline = FALSE)
expect_snapshot_print(tab, label = "ansi-styles_01.md")

options(tinytable_print_output = "dataframe")
expect_snapshot_print(tab, label = "ansi-styles_01.txt")


# bugfix i = NULL applies to colnames
options(tinytable_print_output = "markdown")
tab <- tt(head(iris)) |>
  style_tt(color = "orange") |>
  theme_markdown(ansi = TRUE)
expect_snapshot_print(tab, label = "ansi-style_colnames_01.md")
tab <- tt(head(iris)) |>
  style_tt(i = 0:3, j = 1:2, color = "orange") |>
  theme_markdown(ansi = TRUE)
expect_snapshot_print(tab, label = "ansi-style_colnames_02.md")

# groupj
tab <- tt(head(iris)) |>
  group_tt(j = list("Group1" = 1:2, "Group2" = 3:5)) |>
  style_tt(i = -1, color = "orange") |>
  theme_markdown(ansi = TRUE)
expect_snapshot_print(tab, label = "ansi-style_groupj_01.md")
tab <- tt(head(iris)) |>
  group_tt(j = list("Group1" = 1:2, "Group2" = 3:5)) |>
  style_tt(i = "groupj", color = "orange") |>
  theme_markdown(ansi = TRUE)
expect_snapshot_print(tab, label = "ansi-style_groupj_01.md")


exit_file("broken")
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
