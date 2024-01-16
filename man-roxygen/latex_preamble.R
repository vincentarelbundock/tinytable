#' 
#'
#' @section LaTeX preamble:
#'
#' When rendering Quarto and Rmarkdown documents, `tinytable` will populate the LaTeX preamble automatically with all the required packages. For standalone LaTeX packages, these commands should be inserted in the preamble:
#'
#' ```latex
#' \usepackage{tabularray}
#' \usepackage{float}
#' \usepackage{codehigh}
#' \usepackage[normalem]{ulem}
#' \UseTblrLibrary{booktabs}
#' \newcommand{\tinytableTabularrayUnderline}[1]{\underline{#1}}
#' \newcommand{\tinytableTabularrayStrikeout}[1]{\sout{#1}}
#' \NewTableCommand{\tinytableDefineColor}[3]{\definecolor{#1}{#2}{#3}}
#' ```
#'
