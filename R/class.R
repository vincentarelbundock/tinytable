swap_class <- function(x, new_class) {
  out <- methods::new(new_class)
  for (s in methods::slotNames(x)) {
    methods::slot(out, s) <- methods::slot(x, s)
  }
  return(out)
}


#' tinytable S4 class
#' 
#' @export
setClass(
    Class = "tinytable",
    slots = representation(
        table_dataframe = "data.frame",
        table_string = "character",
        data = "data.frame",
        caption = "character",
        width = "numeric",
        width_cols = "numeric",
        notes = "list",
        theme = "list",
        placement = "character",
        body = "character",
        nrow = "numeric",
        ncol = "numeric",
        nhead = "numeric",
        names = "character",
        output = "character",
        output_dir = "character",
        id = "character",
        bootstrap_class = "character",
        lazy_format = "list",
        lazy_group = "list",
        lazy_style = "list",
        lazy_plot = "list",
        lazy_finalize = "list"
        )
)

setMethod("initialize", "tinytable", function(
    .Object,
    data = data.frame(),
    table = data.frame(),
    caption = NULL,
    notes = NULL,
    theme = list("default"),
    placement = NULL,
    width = NULL) {
  # explicit
  .Object@data <- data
  .Object@table_dataframe <- table
  .Object@theme <- theme
  # dynamic
  .Object@nrow <- nrow(.Object@data)
  .Object@ncol <- ncol(.Object@data)
  .Object@nhead <- if (is.null(colnames(data))) 0 else 1
  .Object@names <- if (is.null(colnames(data))) character() else colnames(data)
  .Object@id <- get_id("tinytable_")
  .Object@output <- "markdown"
  .Object@output_dir <- getwd()
  # conditional: allows NULL user input
  if (!is.null(placement)) .Object@placement <- placement
  if (!is.null(caption)) .Object@caption <- caption
  if (!is.null(width)) .Object@width <- width
  if (!is.null(notes)) .Object@notes <- notes
  return(.Object)
})

setMethod("nrow", "tinytable", function(x) return(x@nrow))
setMethod("ncol", "tinytable", function(x) return(x@ncol))
setMethod("dim", "tinytable", function(x) return(c(x@nrow, x@ncol)))
setMethod("names", "tinytable", function(x) return(x@names))
setMethod("colnames", "tinytable", function(x) return(x@names))
setMethod("as.character", "tinytable", function(x) {
  out <- save_tt(x, x@output)
})


setClass("tinytable_tabularray", contains = "tinytable")
setClass("tinytable_bootstrap", contains = "tinytable")
setClass("tinytable_typst", contains = "tinytable")
setClass("tinytable_grid", contains = "tinytable")

setGeneric(
  name = "style_eval",
  def = function(x, ...) standardGeneric("style_eval")
)

setGeneric(
  name = "tt_eval",
  def = function(x, ...) standardGeneric("tt_eval")
)

setGeneric(
  name = "group_eval",
  def = function(x, ...) standardGeneric("group_eval")
)

setGeneric(
  name = "finalize",
  def = function(x, ...) standardGeneric("finalize")
)
