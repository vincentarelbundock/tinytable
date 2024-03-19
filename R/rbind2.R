#' Combine `tinytable` objects by rows (vertically)
#' 
#' @description
#' Two `tinytable`x
#' objects are combined vertically. 
#'
#' Any `format_tt()` calls applied to `x` or `y` will be evaluated before binding, to allow distinct formatting for each panel.
#'
#' Calls to other `tinytable` function such as `style_tt()` or `group_tt()` are ignored when applied to `x` or `y`. These functions should be applied to the final table instead.
#'
#' @inheritParams data.table::rbindlist
#' @param headers Logical. TRUE inserts the colnames of `y` as an extra row between the two tables.
#' @export
setMethod("rbind2", 
          signature = "tinytable", 
          definition = function(x, y, 
                                fill = TRUE, 
                                use.names = TRUE,
                                headers = FALSE,
                                ...) {

  assert_class(x, "tinytable")
  assert_class(y, "tinytable")
  assert_dependency("data.table")
  assert_flag(fill)
  assert_flag(use.names)
  assert_flag(headers)

  x <- print(x, output = "dataframe")
  y <- print(y, output = "dataframe")

  if (isTRUE(headers)) {
    y <- base::rbind(colnames(y), y)
  }

  out <- data.table::rbindlist(list(x, y), 
                               fill = fill,
                               use.names = use.names)

  out <- tt(out)

  # TODO: restore other lazy things
  return(out)
})
