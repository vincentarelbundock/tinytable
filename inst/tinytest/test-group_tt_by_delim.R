# Works without any delimiters
tab <-
    mtcars |>
    head() |>
    tinytable::tt() |>
    tinytable::group_tt_cols_by_delim()
tinysnapshot::expect_snapshot_print(tab, label = "group_tt_cols_by_delim-no-delim.md")

# Works with delimiters on some columns
tab <-
    data.frame(id = 1, A_a1 = 2, A_a2 = "3", B_b1 = 4, B_b2 = 5, C = 6) |>
    tinytable::tt() |>
    tinytable::group_tt_cols_by_delim()
tinysnapshot::expect_snapshot_print(tab, label = "group_tt_cols_by_delim-some-delim.md")


# Works with alternative delimiter on some columns
tab <-
    data.frame(id = 1, Axa1 = 2, Axa2 = "3", Bxb1 = 4, Bxb2 = 5, C = 6) |>
    tinytable::tt() |>
    tinytable::group_tt_cols_by_delim(delim = "x")
tinysnapshot::expect_snapshot_print(tab, label = "group_tt_cols_by_delim-alt-delim.md")

# Works with selective j
tab <-
    data.frame(id = 1, A_a1 = 2, A_a2 = "3", B_b1 = 4, B_b2 = 5, C = 6) |>
    tinytable::tt() |>
    tinytable::group_tt_cols_by_delim(j = 2:3)
tinysnapshot::expect_snapshot_print(tab, label = "group_tt_cols_by_delim-j-and-delim.md")

# Works with multiple delims (first)
tab <-
    data.frame(id = 1, A_a1_z = 2, A_a2 = "3", B_b1 = 4, B_b2 = 5, C = 6) |>
    tinytable::tt() |>
    tinytable::group_tt_cols_by_delim(split = "first")
tinysnapshot::expect_snapshot_print(tab, label = "group_tt_cols_by_delim-multiple-delims-first.md")

# Works with multiple delims (last)
tab <-
    data.frame(id = 1, A_a1_z = 2, A_a2 = "3", B_b1 = 4, B_b2 = 5, C = 6) |>
    tinytable::tt() |>
    tinytable::group_tt_cols_by_delim(split = "last")
tinysnapshot::expect_snapshot_print(tab, label = "group_tt_cols_by_delim-multiple-delims-last.md")
