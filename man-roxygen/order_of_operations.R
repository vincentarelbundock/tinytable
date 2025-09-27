#' @section Order of Operations:
#'
#' When building tables, tinytable applies operations in this specific order:
#'
#' 1. `format_tt()`: Number and string formatting operations are applied first to ensure all data is properly formatted before any other operations.
#' 2. `group_tt()`: Row and column grouping operations are applied to insert group headers and modify table structure.
#' 3. `theme` argument in `tt()`: The theme specified in the theme argument is applied immediately when creating the table object.
#' 4. `theme_*()` with delayed execution via `build_prepare()`: Theme functions that need to be applied after the table structure is finalized (such as those that depend on final row/column indices) are executed at this stage. Examples include `theme_default()` which applies default borders after groups are inserted. Internally, `build_prepare()` allows calling `style_tt()` with full knowledge of the output format and access to internal slots like `x@output`.
#' 5. `style_tt()` and `theme_*()` with immediate execution: Direct styling calls and theme functions without delayed execution are applied in the order they appear in the code.
#' 6. `theme_*()` with delayed execution via `build_finalize()`: Final theme operations that need to modify the rendered table string are applied last. Examples include `theme_rotate()` which wraps the entire table output. Internally, `build_finalize()` allows post-processing the rendered table in text format using regular expressions and string manipulations.
#'
#' This order ensures that structural changes (grouping) happen before styling, and that operations requiring the final table structure are deferred appropriately. When conflicts arise between operations at the same level, "last applied wins" - the most recent operation takes precedence.

