source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "typst")



# semi complicated
tab <- tt(mtcars[1:4, 1:5], caption = "Hello World") |>
    group_tt(j = list("Group 1" = 4:5, "Group 2" = 2:3)) |>
    style_tt(j = 1:5, align = "lcccr") |>
    style_tt(i = 2, j = 1:3, strikeout = TRUE, bold = TRUE, background = "black", color = "white") |>
    style_tt(j = 1, color = "red", italic = TRUE)
expect_snapshot_print(tab, label = "typst-complicated")


# issue #139
tab <- tt(mtcars[1:10, 1:5]) |>
    group_tt(i = list("Feta" = 2, "Brie" = 6)) |>
    group_tt(j = list("First" = 2:3, "Second" = 4:5)) |>
    style_tt(c(2, 7), align = "c", background = "blue", color = "white")
expect_snapshot_print(tab, label = "typst-issue-139_misaligned_rule_with_group_tt")



options(tinytable_print_output = NULL)