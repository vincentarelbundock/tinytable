#' Markdown theme with optional ANSI color support and grid customization
#'
#' @param x A `tinytable` object.
#' @param ansi Logical. If TRUE, enables ANSI color codes for grid styling. Default is FALSE.
#' @param hline Logical. Enable/disable horizontal lines. Default is TRUE.
#' @param hline_header Logical. Enable/disable the special header separator line below column names. Default is TRUE.
#' @param vline Logical. Enable/disable vertical lines. Default is TRUE.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @details
#' When `ansi = TRUE`, colors and text styling (bold, italic, strikeout, underline) 
#' are applied using ANSI escape sequences for terminal display. ANSI colors require 
#' a terminal or application that supports ANSI escape sequences. Common supported 
#' terminals include: Terminal.app (macOS), iTerm2 (macOS), Windows Terminal, most 
#' Linux terminals, RStudio Console, and VS Code terminal. Colors may not display 
#' correctly in basic text editors or older terminals.
#' @export
theme_markdown <- function(x,
                           ansi = FALSE,
                           hline = TRUE,
                           hline_header = TRUE,
                           vline = TRUE,
                           ...) {
  # Validate logical arguments
  assert_flag(ansi)
  assert_flag(hline)
  assert_flag(hline_header)
  assert_flag(vline)

  # Set the ansi slot based on the argument
  x@ansi <- ansi

  # Set the grid formatting slots
  x@grid_hline <- hline
  x@grid_hline_header <- hline_header
  x@grid_vline <- vline

  return(x)
}

