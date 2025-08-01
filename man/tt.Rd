% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt.R
\name{tt}
\alias{tt}
\title{Draw a Tiny Table}
\usage{
tt(
  x,
  digits = get_option("tinytable_tt_digits", default = NULL),
  caption = get_option("tinytable_tt_caption", default = NULL),
  notes = get_option("tinytable_tt_notes", default = NULL),
  width = get_option("tinytable_tt_width", default = NULL),
  height = get_option("tinytable_tt_height", default = NULL),
  theme = get_option("tinytable_tt_theme", default = "default"),
  colnames = get_option("tinytable_tt_colnames", default = TRUE),
  rownames = get_option("tinytable_tt_rownames", default = FALSE),
  escape = get_option("tinytable_tt_escape", default = FALSE),
  ...
)
}
\arguments{
\item{x}{A data frame or data table to be rendered as a table.}

\item{digits}{Number of significant digits to keep for numeric variables. When \code{digits} is an integer, \code{tt()} calls \code{format_tt(x, digits = digits)} before proceeding to draw the table. Note that this will apply all default argument values of \code{format_tt()}, such as replacing \code{NA} by "". Users who need more control can use the \code{format_tt()} function instead.}

\item{caption}{A string that will be used as the caption of the table. This argument should \emph{not} be used in Quarto or Rmarkdown documents. In that context, please use the appropriate chunk options.}

\item{notes}{Notes to append to the bottom of the table. This argument accepts several different inputs:
\itemize{
\item Single string insert a single note: \code{"blah blah"}
\item Multiple strings insert multiple notes sequentially: \code{list("Hello world", "Foo bar")}
\item A named list inserts a list with the name as superscript: \code{list("a" = list("Hello World"))}
\item A named list with positions inserts markers as superscripts inside table cells: \code{list("a" = list(i = 0:1, j = 2, text = "Hello World"))}
}}

\item{width}{Table or column width.
\itemize{
\item Single numeric value smaller than or equal to 1 determines the full table width, in proportion of line width.
\item Numeric vector of length equal to the number of columns in \code{x} determines the width of each column, in proportion of line width. If the sum of \code{width} exceeds 1, each element is divided by \code{sum(width)}. This makes the table full-width with relative column sizes.
}}

\item{height}{Row height in em units. Single numeric value greater than zero that determines the row height spacing.}

\item{theme}{Function or string.
\itemize{
\item String: bootstrap, grid, multipage, placement, revealjs, resize, rotate, striped, tabular, tabulator, void
\item Function: Applied to the \code{tinytable} object.
}}

\item{colnames}{Logical. If \code{FALSE}, column names are omitted.}

\item{rownames}{Logical. If \code{TRUE}, rownames are included as the first column}

\item{escape}{Logical. If \code{TRUE}, escape special characters in the table. Equivalent to \code{format_tt(tt(x), escape = TRUE)}.}

\item{...}{Additional arguments are ignored}
}
\value{
An object of class \code{tt} representing the table.

The table object has S4 slots which hold information about the structure of the table. For example, the \code{table@group_index_i} slot includes the row indices for grouping labels added by \code{group_tt()}.

Warning: Relying on or modifying the contents of these slots is strongly discouraged. Their names and contents could change at any time, and the \code{tinytable} developers do not consider changes to the internal structure of the output object to be a "breaking  change" for versioning or changelog purposes.
}
\description{
The \code{tt} function renders a table in different formats with various styling options: HTML, Markdown, LaTeX, Word, PDF, PNG, or Typst. The table can be customized with additional functions:
\itemize{
\item \code{style_tt()}: style fonts, colors, alignment, etc.
\item \code{format_tt()}: format numbers, dates, strings, etc.
\item \code{group_tt()}: row or column group labels.
\item \code{theme_tt()}: apply a collection of transformations to a \code{tinytable.}
\item \code{save_tt()}: save the table to a file or return the table as a string.
\item \code{print()}: print to a specific format, ex: \code{print(x, "latex")}
}

\code{tinytable} attempts to determine the appropriate way to print the table based on interactive use, RStudio availability, and output format in RMarkdown or Quarto documents. Users can call \code{print(x, output="markdown")} to print the table in a specific format. Alternatively, they can set a global option: \code{options("tinytable_print_output"="markdown")}
}
\section{Dependencies}{

\itemize{
\item \code{.pdf} output requires a full LaTeX installation on the local computer.
\item \code{.png} output requires the \code{webshot2} package.
\item \code{.html} self-contained files require the \code{base64enc} package.
}
}

\section{LaTeX preamble}{


\code{tinytable} uses the \code{tabularray} package from your LaTeX distribution to draw tables. \code{tabularray}, in turn, uses the special \code{tblr}, \code{talltblr}, and \code{longtblr} environments.

When rendering a document from Quarto or Rmarkdown directly to PDF, \code{tinytable} will populate the LaTeX preamble automatically with all the required packages. For standalone LaTeX documents, these commands should be inserted in the preamble manually:

Note: Your document will fail to compile to PDF in Quarto if you enable caching and you use tinytable due to missing LaTeX headers. To avoid this problem, set the option \verb{#| cache: false} for the chunk(s) where you use tinytable.

\if{html}{\out{<div class="sourceCode latex">}}\preformatted{\\usepackage\{tabularray\}
\\usepackage\{float\}
\\usepackage\{graphicx\}
\\usepackage\{rotating\}
\\usepackage[normalem]\{ulem\}
\\UseTblrLibrary\{booktabs\}
\\UseTblrLibrary\{siunitx\}
\newcommand{\tinytableTabularrayUnderline}[1]\{\\underline\{#1\}\}
\newcommand{\tinytableTabularrayStrikeout}[1]\{\\sout\{#1\}\}
\\NewTableCommand\{\\tinytableDefineColor\}[3]\{\\definecolor\{#1\}\{#2\}\{#3\}\}
}\if{html}{\out{</div>}}
}

\section{Word and Markdown limitations}{


Markdown and Word tables only support these styles: italic, bold, strikeout. The \code{width} argument is also unavailable
Moreover, the \code{style_tt()} function cannot be used to style headers inserted by the \code{group_tt()} function;
instead, you should style the headers directly in the header definition using markdown syntax:
\code{group_tt(i = list("*italic header*" = 2))}. These limitations are due to the fact that there is no markdown
syntax for the other options, and that we create Word documents by converting a markdown table to .docx
via the Pandoc software.
}

\section{Global options}{


Options can be set with \code{options()} and change the default behavior of tinytable. For example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{options(tinytable_tt_digits = 4)
tt(head(iris))
}\if{html}{\out{</div>}}

You can set options in a script or via \code{.Rprofile}. Note: be cautious with \code{.Rprofile} settings as they may affect reproducibility.
\subsection{Default values for function arguments}{
\subsection{tt()}{
\itemize{
\item \code{tinytable_tt_digits}
\item \code{tinytable_tt_caption}
\item \code{tinytable_tt_notes}
\item \code{tinytable_tt_width}
\item \code{tinytable_tt_theme}
\item \code{tinytable_tt_rownames}
}
}

\subsection{format_tt()}{
\itemize{
\item \code{tinytable_format_digits}
\item \code{tinytable_format_num_fmt}
\item \code{tinytable_format_num_zero}
\item \code{tinytable_format_num_suffix}
\item \code{tinytable_format_num_mark_big}
\item \code{tinytable_format_num_mark_dec}
\item \code{tinytable_format_date}
\item \code{tinytable_format_bool}
\item \code{tinytable_format_other}
\item \code{tinytable_format_replace}
\item \code{tinytable_format_escape}
\item \code{tinytable_format_markdown}
\item \code{tinytable_format_quarto}
\item \code{tinytable_format_fn}
\item \code{tinytable_format_sprintf}
}
}

\subsection{save_tt()}{
\itemize{
\item \code{tinytable_save_overwrite}
}
}

\subsection{theme_tt()}{

Placement:
\itemize{
\item \code{tinytable_theme_placement_float}
\item \code{tinytable_theme_placement_horizontal}
}

Resize:
\itemize{
\item \code{tinytable_theme_resize_width}
\item \code{tinytable_theme_resize_direction}
}

Multipage:
\itemize{
\item \code{tinytable_theme_multipage_rowhead}
\item \code{tinytable_theme_multipage_rowfoot}
}

Tabular:
\itemize{
\item \code{tinytable_theme_tabular_style}
}
}

\subsection{print.tinytable()}{
\itemize{
\item \code{tinytable_print_output}
}
}

}

\subsection{Output-specific options}{
\subsection{HTML}{
\itemize{
\item \code{tinytable_html_mathjax}: Insert MathJax scripts (warning: may conflict if MathJax is loaded elsewhere)
\item \code{tinytable_html_portable}: Insert base64 encoded images directly in HTML for \code{plot_tt()}
\item \code{tinytable_html_engine}: Default HTML engine (default: "bootstrap"). Set to "tabulator" to use interactive tables by default in HTML documents instead of static Bootstrap tables.
}
}

\subsection{PDF}{
\itemize{
\item \code{tinytable_pdf_clean}: Delete temporary and log files
\item \code{tinytable_pdf_engine}: Choose between "xelatex", "pdflatex", "lualatex"
}
}

}

\subsection{Color processing}{
\itemize{
\item \code{tinytable_color_name_normalization}: Enable/disable automatic color name processing (default: TRUE). When enabled, R color names recognized by \code{col2rgb()} are converted to hex format for consistent rendering across HTML, LaTeX, and Typst formats. If R color conversion fails, LaTeX color names are used as fallback. Colors explicitly supplied as hex values with "#" prefix are passed through unchanged. Set to FALSE to disable processing and pass color names unchanged.
}
\subsection{Quarto}{

The \code{format_tt(quarto=TRUE)} argument enables Quarto data processing with some limitations:
\enumerate{
\item The \verb{\\QuartoMarkdownBase64\{\}} LaTeX macro may not process references and markdown as expected
\item Quarto processing may conflict with \code{tinytable} styling/formatting
}

Options:
\itemize{
\item \code{tinytable_quarto_disable_processing}: Disable Quarto cell processing
\item \code{tinytable_print_rstudio_notebook}: Display tables "inline" or in "viewer" for RStudio notebooks
\item \code{tinytable_quarto_figure}: Control Typst figure environment in Quarto
}

Example of Quarto-specific code in cells:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{x <- data.frame(Math = "x^2^", Citation = "@Lovelace1842")
fn <- function(z) sprintf("<span data-qmd='\%s'></span>", z)
tt(x) |> format_tt(i = 1, fn = fn)
}\if{html}{\out{</div>}}

For more details on Quarto table processing: https://quarto.org/docs/authoring/tables.html#disabling-quarto-table-processing
}

}
}

\section{Interactive tables with Tabulator}{


\strong{Experimental Feature:} The Tabulator.js integration is experimental and the API may change in future versions.

The Tabulator.js library provides powerful interactive table features including sorting, filtering, pagination, data export, and real-time editing capabilities. This theme customizes the appearance and behavior of Tabulator tables.

\strong{Features:}
\itemize{
\item Sorting and filtering of all columns
\item Pagination with configurable page sizes
\item Search functionality across all columns
\item Multiple CSS themes and custom styling
\item Real-time data export options
\item Accessibility features (ARIA compliant)
}

\strong{Limitations:}
\itemize{
\item Limited \code{style_tt()} support (only \code{align} and \code{alignv})
\item Row-based formatting (\code{format_tt()} with \code{i} argument) not supported
\item Global stylesheets affect all tables in multi-table documents
\item Date formatting uses Luxon tokens, not R's \code{strptime} format
\item Boolean formatting requires \code{format_tt()} with \code{bool} argument for custom display
}
}

\examples{
library(tinytable)
x <- mtcars[1:4, 1:5]

tt(x)

tt(x,
  theme = "striped",
  width = 0.5,
  caption = "Data about cars."
)

tt(x, notes = "Hello World!")

fn <- list(i = 0:1, j = 2, text = "Hello World!")
tab <- tt(x, notes = list("*" = fn))
print(tab, "latex")

k <- data.frame(x = c(0.000123456789, 12.4356789))
tt(k, digits = 2)

}
