# Unit tests for internal helpers in R/utils.R
lines_drop <- tinytable:::lines_drop
lines_drop_between <- tinytable:::lines_drop_between

txt <- "alpha\nbravo\ncharlie\ndelta"

# lines_drop_between: start marker on the first line
expect_equal(lines_drop_between(txt, "alpha", "bravo"), "charlie\ndelta")

# lines_drop_between: end marker on the last line
expect_equal(lines_drop_between(txt, "charlie", "delta"), "alpha\nbravo")

# lines_drop: no match is a no-op for all four positions
for (pos in c("equal", "before", "after", "all")) {
  expect_equal(lines_drop(txt, "zzz", position = pos), txt)
}

# get_id() must not disturb the user's RNG state (building an HTML table
# calls get_id() internally)
set.seed(1)
r1 <- rnorm(1)
set.seed(1)
void <- tinytable::save_tt(tinytable::tt(data.frame(x = 1)), "html")
r2 <- rnorm(1)
expect_equal(r1, r2)
