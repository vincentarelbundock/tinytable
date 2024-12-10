#' @section Word and Markdown limitations:
#'
#' Markdown and Word tables only support these styles: italic, bold, strikeout. The `width` arugment is also unavailable
#' Moreover, the `style_tt()` function cannot be used to style headers inserted by the `group_tt()` function;
#' instead, you should style the headers directly in the header definition using markdown syntax:
#' `group_tt(i = list("*italic header*" = 2))`. These limitations are due to the fact that there is no markdown
#' syntax for the other options, and that we create Word documents by converting a markdown table to .docx
#' via the Pandoc software.
