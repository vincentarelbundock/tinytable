#' @section Order of Operations:
#'
#' The specified order of operations used when building tables is defined in the `build.R` file, which can be viewed on Github: https://github.com/vincentarelbundock/tinytable/tree/main/R
#'
#' A few important things to note:
#'
#' * The `i` and `j` indices in `format_tt()` and `style_tt()` refer to the table structure *after* any grouping operations have been applied.
#' * The `theme` argument in `tt()` is applied before any other theme, and pre-empts the default theme.
#' * Theme functions apply transformations immediately when they are called, but they can delay the execution of certain operations using internal helpers: `build_prepare()` and `build_finalize()`. See `theme_*()` files in the Github repository for examples.
#' * `rbind()` and `rbind2()` combine two `tinytable` objects before formatting. In some cases, this has the undesirable consequence of coercing numeric variables to character, which prevents further numeric formatting. To avoid this, users can apply `format_tt()` directly to the data frames before calling `tt()`. 
