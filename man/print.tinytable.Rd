% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.tinytable}
\alias{print.tinytable}
\title{Print, display, or convert a tinytable object}
\usage{
\method{print}{tinytable}(x, output = get_option("tinytable_print_output", default = NULL), ...)
}
\arguments{
\item{x}{A data frame or data table to be rendered as a table.}

\item{output}{format in which a Tiny Table is printed: \code{NULL} or one of \code{"latex"}, \code{"markdown"}, \code{"html"}, \code{"typst"}, \code{"dataframe"}, \code{"tabulator"}. If \code{NULL}, the output is chosen based on these rules:
\itemize{
\item When called from a script in non-interactive mode, the default is "markdown" (\code{interactive() == FALSE}).
\item When called interactively in RStudio, the default is to display an HTML table in the viewer pane.
\item When called interactively in another development environment, the default is "markdown".
\item The default print output can be changed for an entire R session by calling: \code{options(tinytable_print_output = "html")}
\item The default print output can be changed for a single \code{tinytable} object by modifying the \code{output} S4 slot.
}}

\item{...}{Other arguments are ignored.}
}
\value{
launch a browser window or cat() the table to console.
}
\description{
This function is called automatically by \code{R} whenever a \code{tinytable} object is anprinted to the console or in an HTML viewer pane.
}
