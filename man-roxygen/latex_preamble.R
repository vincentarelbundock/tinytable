#'
#'
#' @section LaTeX preamble:
#'
#' `tinytable` uses the `tabularray` package from your LaTeX distribution to draw tables. `tabularray`, in turn, uses the special `tblr`, `talltblr`, and `longtblr` environments.
#'
#' When rendering a document from Quarto or Rmarkdown directly to PDF, `tinytable` will populate the LaTeX preamble automatically with all the required packages. For standalone LaTeX documents, these commands should be inserted in the preamble manually:
#'
#' Note: Your document will fail to compile to PDF in Quarto if you enable caching and you use tinytable due to missing LaTeX headers. To avoid this problem, set the option `#| cache: false` for the chunk(s) where you use tinytable.
#'
#' ```latex
#' \usepackage{tabularray}
#' \usepackage{float}
#' \usepackage{graphicx}
#' \usepackage{rotating}
#' \usepackage[normalem]{ulem}
#' \UseTblrLibrary{siunitx}
#' \newcommand{\tinytableTabularrayUnderline}[1]{\underline{#1}}
#' \newcommand{\tinytableTabularrayStrikeout}[1]{\sout{#1}}
#' \NewTableCommand{\tinytableDefineColor}[3]{\definecolor{#1}{#2}{#3}}
#' ```
#'
