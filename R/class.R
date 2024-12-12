swap_class <- function(x, new_class) {
  out <- methods::new(new_class)
  for (s in methods::slotNames(x)) {
    # modelsummary issue #727
    tmp <- methods::slot(x, s)
    if (inherits(tmp, "data.table")) {
      assert_dependency("data.table")
      data.table::setDT(tmp)
    }
    methods::slot(out, s) <- tmp
  }
  return(out)
}

setClassUnion("NULLorCharacter", c("NULL", "character"))

#' tinytable S4 class
#'
#' @keywords internal
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
    ngroupi = "numeric",
    ngroupj = "numeric",
    group_i_idx = "numeric",
    names = "NULLorCharacter",
    output = "character",
    output_dir = "character",
    id = "character",
    bootstrap_class = "character",
    bootstrap_css_rule = "character",
    css = "data.frame",
    style = "data.frame",
    style_caption = "list",
    style_notes = "list",
    lazy_format = "list",
    lazy_group = "list",
    lazy_style = "list",
    lazy_plot = "list",
    lazy_finalize = "list",
    lazy_theme = "list",
    portable = "logical"
  )
)

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
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
  .Object@ngroupi <- 0
  .Object@ngroupj <- 0
  .Object@names <- if (is.null(colnames(data))) character() else colnames(data)
  .Object@id <- get_id("tinytable_")
  .Object@output <- "tinytable"
  .Object@output_dir <- getwd()
  .Object@css <- data.frame(i = NA, j = NA, bootstrap = NA, id = NA)
  .Object@portable <- FALSE
  .Object@style <- data.frame()
  .Object@lazy_theme <- list()
  # conditional: allows NULL user input
  if (!is.null(placement)) .Object@placement <- placement
  if (!is.null(caption)) .Object@caption <- caption
  if (!is.null(width)) .Object@width <- width
  if (!is.null(notes)) .Object@notes <- notes
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
setReplaceMethod("colnames",
  signature = "tinytable",
  definition = function(x, value) {
    # Issue #306
    if (length(value) == 0) value <- NULL
    if (!is.null(value)) {
      assert_character(value, len = length(x@names))
    } else {
      if (x@nhead == 1) x@nhead <- 0
    }
    x@names <- value
    return(x)
  }
)

#' Method for a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
#' @export
setReplaceMethod("names",
  signature = "tinytable",
  definition = function(x, value) {
    # Issue #306
    if (length(value) == 0) value <- NULL
    if (!is.null(value)) {
      assert_character(value, len = length(x@names))
    } else {
      if (x@nhead == 1) x@nhead <- 0
    }
    x@names <- value
    return(x)
  }
)

#' Dimensions a tinytable S4 object
#'
#' @inheritParams tt
#' @keywords internal
setMethod("dim", "tinytable", function(x) {
  return(c(x@nrow, x@ncol))
})

#' Column names of a tinytable
#'
#' @inheritParams tt
#' @keywords internal
setMethod("names", "tinytable", function(x) {
  return(x@names)
})

#' Convert a tinytable S4 object to a string
#'
#' @inheritParams tt
#' @keywords internal
setMethod("as.character", "tinytable", function(x) {
  out <- save_tt(x, x@output)
})


setClass("tinytable_tabularray", contains = "tinytable")
setClass("tinytable_bootstrap", contains = "tinytable")
setClass("tinytable_typst", contains = "tinytable")
setClass("tinytable_grid", contains = "tinytable")
setClass("tinytable_dataframe", contains = "tinytable")

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
  name = "tt_eval",
  def = function(x, ...) standardGeneric("tt_eval")
)

#' Apply group settings to a tinytable
#'
#' @inheritParams tt
#' @keywords internal
setGeneric(
  name = "group_eval",
  def = function(x, ...) standardGeneric("group_eval")
)

#' Apply final settings to a tinytable
#'
#' @inheritParams tt
#' @keywords internal
setGeneric(
  name = "finalize",
  def = function(x, ...) standardGeneric("finalize")
)
