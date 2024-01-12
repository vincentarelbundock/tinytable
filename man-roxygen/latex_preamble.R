#' 
#'
#' @section LaTeX preamble:
#'
#' To be able to use all features of `tinytable` in PDF (LaTeX) documents, these commands need to be placed in your LaTeX preamble:
#'
#' ```latex
#' \usepackage{float}
#' \usepackage{codehigh}
#' \usepackage{tabularray}
#' \UseTblrLibrary{booktabs}
#' \NewTableCommand{\tinytableDefineColor}[3]{\definecolor{#1}{#2}{#3}}
#' ```
#'
