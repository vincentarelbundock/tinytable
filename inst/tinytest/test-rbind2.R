source("helpers.R")
using("tinysnapshot")

# Examples from @examples section in rbind2.R
x <- tt(mtcars[1:3, 1:2], caption = "Combine two tiny tables.")
y <- tt(mtcars[4:5, 8:10])

# basic combination using rbind()
tab1 <- rbind(x, y)
expect_snapshot_print(tab1, label = "rbind2-basic_rbind.md")

# basic combination with format_tt
tab2 <- rbind(x, y) |> format_tt(replace = "")
expect_snapshot_print(tab2, label = "rbind2-basic_rbind_format.md")

# rbind2() with headers = FALSE (omit y header)
tab3 <- rbind2(x, y, headers = FALSE)
expect_snapshot_print(tab3, label = "rbind2-no_headers.md")

# rbind2() with use_names = FALSE (bind by position rather than column names)
tab4 <- rbind2(x, y, use_names = FALSE)
expect_snapshot_print(tab4, label = "rbind2-position_bind.md")

# Examples from vignettes (using a, b variable names like in vignette)
a <- tt(mtcars[1:3, 1:2], caption = "Combine two tiny tables.")
b <- tt(mtcars[4:5, 8:10])

# vignette rbind() example
tab5 <- rbind(a, b)
expect_snapshot_print(tab5, label = "rbind2-vignette_basic.md")

# vignette rbind() with format_tt
tab6 <- rbind(a, b) |> format_tt(replace = "")
expect_snapshot_print(tab6, label = "rbind2-vignette_format.md")

# vignette rbind2() with headers = FALSE
tab7 <- rbind2(a, b, headers = FALSE)
expect_snapshot_print(tab7, label = "rbind2-vignette_no_headers.md")

# vignette rbind2() with use_names = FALSE
tab8 <- rbind2(a, b, use_names = FALSE)
expect_snapshot_print(tab8, label = "rbind2-vignette_position.md")

# Test with identical column names
c <- tt(mtcars[1:2, 1:3])
d <- tt(mtcars[6:7, 1:3])
tab9 <- rbind2(c, d)
expect_snapshot_print(tab9, label = "rbind2-identical_columns.md")

# Test with notes carrying over
e <- tt(mtcars[1:2, 1:2], notes = "First table note")
f <- tt(mtcars[3:4, 1:2], notes = "Second table note")
tab10 <- rbind2(e, f)
expect_snapshot_print(tab10, label = "rbind2-notes_carryover.md")