#'
#'
#' @section LaTeX preamble:
#'
#' `tinytable` uses the `tabularray` package from your LaTeX distribution to draw tables. `tabularray`, in turn, uses the special `tblr`, `talltblr`, and `longtblr` environments.
#'
#' When rendering a document from Quarto or Rmarkdown directly to PDF, `tinytable` will populate the LaTeX preamble automatically with all the required packages (except when code chunks are cached). For standalone LaTeX documents, these commands should be inserted in the preamble manually:
#'
#' ```latex
#' \usepackage{tabularray}
#' \usepackage{float}
#' \usepackage{graphicx}
#' \usepackage[normalem]{ulem}
#' \UseTblrLibrary{booktabs}
#' \UseTblrLibrary{siunitx}
#' \newcommand{\tinytableTabularrayUnderline}[1]{\underline{#1}}
#' \newcommand{\tinytableTabularrayStrikeout}[1]{\sout{#1}}
#' \NewTableCommand{\tinytableDefineColor}[3]{\definecolor{#1}{#2}{#3}}
#' ```
#'
