#' @section Markdown limitations:
#'
#' Markdown is a text-only format that only supports these styles: italic, bold, strikeout. The `width` argument is also unavailable.
#' These limitations exist because there is no standard markdown syntax for the other styling options.
#' 
#' However, in terminals (consoles) that support it, `tinytable` can display colors and text styles using 
#' ANSI escape codes by setting `theme_markdown(ansi = TRUE)`. This allows for rich formatting in 
#' compatible terminal environments.
#'
#' @section Word limitations:
#'
#' Word tables only support these styles: italic, bold, strikeout. The `width` argument is also unavailable.
#' Moreover, the `style_tt()` function cannot be used to style headers inserted by the `group_tt()` function;
#' instead, you should style the headers directly in the header definition using markdown syntax:
#' `group_tt(i = list("*italic header*" = 2))`. These limitations are due to the fact that we create 
#' Word documents by converting a markdown table to .docx via the Pandoc software, which requires 
#' going through a text-only intermediate format.
