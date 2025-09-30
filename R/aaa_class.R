swap_class <- function(x, new_class) {
  class(x) <- new_class
  return(x)
}

setClassUnion("NULLorCharacter", c("NULL", "character"))
setClassUnion("NULLorNumeric", c("NULL", "numeric"))
setClassUnion("NULLorList", c("NULL", "list"))

#' tinytable S4 class
#'
#' @keywords internal
#' @export
setClass(
  Class = "tinytable",
  slots = representation(
    ansi = "logical",
    body = "character",
    html_class = "character",
    html_css_rule = "NULLorCharacter",
    caption = "NULLorCharacter",
    css = "data.frame",
    data = "data.frame",
    data_body = "data.frame",
    grid_hline = "logical",
    grid_hline_header = "logical",
    grid_vline = "logical",
    group_data_i = "data.frame",
    group_data_j = "data.frame",
    group_index_i = "numeric",
    height = "NULLorNumeric",
    id = "character",
    index_body = "numeric",
    lazy_finalize = "list",
    lazy_format = "list",
    lazy_plot = "list",
    lazy_prepare = "list",
    lazy_style = "list",
    lazy_subset = "ANY",
    markdown_style = "character",
    names = "NULLorCharacter",
    ncol = "numeric",
    nhead = "numeric",
    notes = "NULLorList",
    nrow = "numeric",
    output = "character",
    output_dir = "character",
    placement = "NULLorCharacter",
    html_portable = "logical",
    html_engine = "character",
    latex_preamble = "logical",
    latex_engine = "character",
    style = "data.frame",
    style_caption = "list",
    style_notes = "list",
    table_string = "character",
    tabularray_inner = "character",
    tabularray_outer = "character",
    tabulator_format_bool = "logical",
    tabulator_column_formatters = "list",
    tabulator_column_styles = "list",
    tabulator_columns = "list",
    tabulator_css_rule = "character",
    tabulator_options = "character",
    tabulator_post_init = "character",
    tabulator_search = "NULLorCharacter",
    tabulator_stylesheet = "character",
    theme = "list",
    width = "NULLorNumeric",
    width_cols = "numeric"
  )
)

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
setMethod(
  "initialize",
  "tinytable",
  function(.Object,
           data = data.frame(),
           caption = NULL,
           notes = NULL,
           theme = list("default"),
           data_body = data.frame(),
           placement = NULL,
           width = NULL,
           height = NULL,
           colnames = TRUE) {
    # explicit
    .Object@data <- data
    .Object@data_body <- data_body
    .Object@theme <- theme
    .Object@placement <- placement
    .Object@caption <- caption
    .Object@width <- width
    .Object@notes <- notes
    .Object@height <- height
    # Generate unique ID first
    .Object@id <- get_id("tinytable_")

    # Default to NULL - framework CSS will be loaded externally
    .Object@html_css_rule <- NULL

    # dynamic
    .Object@nrow <- nrow(.Object@data)
    .Object@ncol <- ncol(.Object@data)
    .Object@nhead <- if (is.null(colnames(data)) || isFALSE(colnames)) 0 else 1

    # colnames
    if (isTRUE(colnames)) {
      .Object@names <- colnames(data)
    } else if (isFALSE(colnames) || is.null(colnames(data))) {
      .Object@names <- character()
    } else if (identical(colnames, "label")) {
      cols <- character()
      for (cn in colnames(data)) {
        lab <- attr(data[[cn]], "label")
        if (!is.null(lab)) {
          cols <- c(cols, lab)
        } else {
          cols <- c(cols, cn)
        }
      }
      .Object@names <- cols
    }

    # empty
    .Object@ansi <- FALSE
    .Object@css <- data.frame(i = NA, j = NA, html = NA, id = NA)
    .Object@grid_hline <- TRUE
    .Object@grid_hline_header <- TRUE
    .Object@grid_vline <- TRUE
    .Object@group_data_i <- data.frame()
    .Object@group_data_j <- data.frame()
    .Object@group_index_i <- numeric(0)
    .Object@html_class <- "tinytable"
    .Object@html_engine <- "tinytable"
    .Object@html_portable <- FALSE
    .Object@index_body <- numeric(0)
    .Object@latex_engine <- "xelatex"
    .Object@latex_preamble <- TRUE
    .Object@lazy_finalize <- list()
    .Object@lazy_format <- list()
    .Object@lazy_plot <- list()
    .Object@lazy_prepare <- list()
    .Object@lazy_subset <- NULL
    .Object@markdown_style <- "grid"
    .Object@output <- "tinytable"
    .Object@output_dir <- getwd()
    .Object@style <- data.frame()
    .Object@tabularray_inner <- character()
    .Object@tabularray_outer <- character()
    .Object@tabulator_column_formatters <- list()
    .Object@tabulator_column_styles <- list()
    .Object@tabulator_columns <- list()
    .Object@tabulator_css_rule <- ""
    .Object@tabulator_format_bool <- FALSE
    .Object@tabulator_options <- ""
    .Object@tabulator_post_init <- ""
    .Object@tabulator_search <- NULL
    .Object@tabulator_stylesheet <- ""

    return(.Object)
  })

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
setMethod("nrow", "tinytable", function(x) {
  return(x@nrow)
})

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
setMethod("ncol", "tinytable", function(x) {
  return(x@ncol)
})

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
#' @export
setMethod("colnames", "tinytable", function(x) {
  return(x@names)
})

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
#' @export
setMethod("names", "tinytable", function(x) {
  return(x@names)
})

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
#' @export
setReplaceMethod(
  "colnames",
  signature = "tinytable",
  definition = function(x, value) {
    # Issue #306
    if (length(value) == 0) {
      value <- NULL
    }
    if (!is.null(value)) {
      assert_character(value, len = length(x@names))
    } else {
      if (x@nhead == 1) x@nhead <- 0
    }
    x@names <- value
    return(x)
  })

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
#' @export
setReplaceMethod(
  "names",
  signature = "tinytable",
  definition = function(x, value) {
    # Issue #306
    if (length(value) == 0) {
      value <- NULL
    }
    if (!is.null(value)) {
      assert_character(value, len = length(x@names))
    } else {
      if (x@nhead == 1) x@nhead <- 0
    }
    x@names <- value
    return(x)
  })

#' Dimensions a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
setMethod("dim", "tinytable", function(x) {
  return(c(x@nrow, x@ncol))
})

#' Convert a tinytable S4 object to a string
#'
#' @inheritParams tt
#' @keywords internal
setMethod("as.character", "tinytable", function(x) {
  out <- save_tt(x, x@output)
})

setClass("tinytable_tabularray", contains = "tinytable")
setClass("tinytable_html", contains = "tinytable")
setClass("tinytable_typst", contains = "tinytable")
setClass("tinytable_grid", contains = "tinytable")
setClass("tinytable_dataframe", contains = "tinytable")
setClass("tinytable_tabulator", contains = "tinytable")

#' Apply style settings to a tinytable
#'
#' @inheritParams tt
#' @keywords internal
setGeneric(
  name = "style_eval",
  def = function(x, ...) standardGeneric("style_eval")
)

#' Apply group settings to a tinytable
#'
#' @inheritParams tt
#' @keywords internal
setGeneric(
  name = "build_eval",
  def = function(x, ...) standardGeneric("build_eval")
)

#' Apply group settings to a tinytable
#'
#' @inheritParams tt
#' @keywords internal
setGeneric(
  name = "group_eval_j",
  def = function(x, ...) standardGeneric("group_eval_j")
)


#' Apply final settings to a tinytable
#'
#' @inheritParams tt
#' @keywords internal
setGeneric(
  name = "finalize",
  def = function(x, ...) standardGeneric("finalize")
)
