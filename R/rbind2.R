#' Combine `tinytable` objects by rows (vertically)
#'
#' @details
#' Transformations recorded via `format_tt()` and `style_tt()` are evaluated at the very end of the rendering pipeline, after `rbind2()` has combined the tables. When headers are inserted or columns differ in type, the combined data is first coerced to character, so subsequent formatting/styling works on strings. Apply `format_tt()` directly to raw data frames before calling `tt()`, or re-run the formatting/styling steps on the combined table to preserve rounding and other rules.
#'
#' Calls to other `tinytable` functions such as `style_tt()` or `group_tt()` are ignored when applied to `x` or `y`. These functions should be applied to the final table instead.
#'
#' Information in these S4 slots is carried over from `x` to the combined table:
#'
#' + `x@output`
#' + `x@caption`
#' + `x@width`
#'
#' Information in these S4 slots is concatenated and carried over to the combined table:
#'
#' + `c(x@notes, y@notes)`
#'
#' This function relies on the `rbindlist()` function from the `data.table` package.
#'
#' @param x `tinytable` object
#' @param y `tinytable` object
#' @param use_names ‘TRUE’ binds by matching column name, ‘FALSE’ by position
#' @param headers Logical. TRUE inserts the colnames of `y` as an extra row between the two tables.
#' @param ... Additional arguments are ignored.
#' @aliases rbind2
#' @examples
#' library(tinytable)
#' x <- tt(mtcars[1:3, 1:2], caption = "Combine two tiny tables.")
#' y <- tt(mtcars[4:5, 8:10])
#'
#' # rbind() does not support additional aarguments
#' # rbind2() supports additional arguments
#'
#' # basic combination
#' rbind(x, y)
#'
#' rbind(x, y) |> format_tt(replace = "")
#'
#' # omit y header
#' rbind2(x, y, headers = FALSE)
#'
#' # bind by position rather than column names
#' rbind2(x, y, use_names = FALSE)
#'
#' # `iris` example with pre-tt() formatting
#' dat <- iris[1:3, 1:4]
#' a <- format_tt(dat, i = 1:3, digits = 1) |> tt()
#' b <- format_tt(dat, i = 1:3, digits = 2) |> tt()
#' rbind2(a, b)
#'
#' @importFrom methods rbind2
#' @export
setMethod(
  "rbind2",
  signature = signature(x = "tinytable", y = "tinytable"),
  definition = function(x, y, use_names = TRUE, headers = TRUE, ...) {
    assert_class(x, "tinytable")
    assert_class(y, "tinytable")
    assert_dependency("data.table")
    assert_flag(use_names)
    assert_flag(headers)

    x_df <- x@data_body
    y_df <- y@data_body

    if (isTRUE(headers) && !is.null(colnames(y))) {
      y_df <- base::rbind(colnames(y_df), y_df)
    }

    out <- data.table::rbindlist(
      list(x_df, y_df),
      fill = TRUE,
      use.names = use_names
    )

    out <- tt(out)

    out@output <- x@output
    out@notes <- c(x@notes, y@notes)
    out@width <- x@width
    out@caption <- x@caption

    return(out)
  }
)
