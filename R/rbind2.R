#' Combine `tinytable` objects by rows (vertically)
#' 
#' @details
#' `format_tt()` calls applied to `x` or `y` are evaluated before binding, to allow distinct formatting for each panel.
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
#' x = tt(mtcars[1:3, 1:2], caption = "Combine two tiny tables.")
#' y = tt(mtcars[4:5, 8:10]) 
#' 
#' # rbind() does not support additional aarguments
#' # rbind2() supports additional arguments
#' 
#' # basic combination
#' rbind(x, y)
#' 
#' rbind(x, y) |> format_tt(replace_na = "")
#' 
#' # omit y header
#' rbind2(x, y, headers = FALSE)
#' 
#' # bind by position rather than column names
#' rbind2(x, y, use_names = FALSE)
#' 
#' @importFrom methods rbind2
#' @export
setMethod("rbind2", 
          signature = "tinytable", 
          definition = function(x, y, 
                                use_names = TRUE,
                                headers = TRUE,
                                ...) {

  assert_class(x, "tinytable")
  assert_class(y, "tinytable")
  assert_dependency("data.table")
  assert_flag(use_names)
  assert_flag(headers)

  x_df <- print(x, output = "dataframe")
  y_df <- print(y, output = "dataframe")

  if (isTRUE(headers) && !is.null(colnames(y))) {
    y_df <- base::rbind(colnames(y_df), y_df)
  }

  out <- data.table::rbindlist(list(x_df, y_df), 
                               fill = TRUE,
                               use.names = use_names)

  out <- tt(out)

  out@output <- x@output
  out@notes <- c(x@notes, y@notes)
  out@width <- x@width
  out@caption <- x@caption

  return(out)
})
