#' LaTeX options
#'
#' @param environment TODO
#' @param extendable TODO
#' @param placement TODO
#' @param theme TODO
#' @template tabularray
#' @export
latexOptions <- function(
  environment = getOption("tt_latexOptions_environment", default = "table+tblr"),
  extendable = getOption("tt_latexOptions_extendable", default = FALSE),
  placement = getOption("tt_latexOptions_placement", default = NULL),
  theme = getOption("tt_latexOptions_theme", default = "booktabs"),
  ...) {

  dots <- list(...)

  assert_choice(environment, c("table+tblr", "tblr"))
  assert_flag(extendable)
  assert_string(placement, null.ok = TRUE)
  assert_choice(theme, c("booktabs", "void", "grid"))

  if (environment %in% c("table+tblr", "tbrl")) {
    template <- readLines(system.file("templates/tblr.tex", package = "tinytable"))
  }

  if (is.null(placement)) {
    placement <- ""
  } else {
    placement <- sprintf("[%s]", placement)
  }
  template <- sub("$TINYTABLE_PLACEMENT", placement, template, fixed = TRUE)

  # TRUE -> true
  if ("endpos" %in% names(dots)) {
    dots$endpos <- tolower(dots$endpos)
  }

  # clean dots
  # loop over indices to allow duplicate keys like vline
  for (i in seq_along(dots)) {
    n <- names(dots)[i]

    if (n %in% c("vline", "hline")) {
      if (!is.character(dots[[i]]) || length(dots[[i]]) != 2) {
        msg <- "The `hline` and `vline` keys must be character vectors of length 2, ex: `hline=c('2,3', 'orange,2pt')`"
        stop(msg, call. = FALSE)
      }
      dots[[i]] <- sprintf("%s{%s}={%s}", n, dots[[i]][1], dots[[i]][2])

    } else if (n %in% c("vlines", "hlines", "preto", "appto")) {
      dots[[i]] <- sprintf("%s={%s}", n, dots[[i]])

    } else {
      dots[[n]] <- paste0(n, "=", dots[[i]])
    }
  }
  
  out <- list(
    rows_keys = c("halign", "valign", "ht", "bg", "fg", "font", "mode", "cmd", "abovesep", "belowsep", "rowsep", "preto", "appto", "indent"),
    columns_keys = c("halign", "valign", "wd", "co", "bg", "fg", "font", "mode", "cmd", "leftsep", "rightsep", "colsep", "preto", "appto", "indent"),
    hborders_keys = c("pagebreak", "abovespace", "belowspace"),
    vborders_keys = c("leftspace", "rightspace"),
    cells_keys = c("halign", "valign", "wd", "bg", "fg", "font", "mode", "cmd", "preto", "appto"),
    outer_specs_keys = c("baseline", "long", "tall", "expand"),
    inner_specs_keys = c("rulesep", "hlines", "vline", "hline", "vlines", "stretch", "abovesep", "belowsep", "rowsep", "leftsep", "rightsep", "colsep", "hspan", "vspan", "baseline"),
    span = c("r", "c")
  )

  # do not select by name to allow duplicate keys like hline
  for (i in seq_along(out)) {
    idx <- names(dots) %in% out[[i]]
    out[[i]] <- paste(dots[idx], collapse = ",")
  }

  dots <- list(
    environment = environment,
    extendable = extendable,
    template = template,
    theme = theme,
    keys = dots
  )
  dots <- c(dots, out)
  class(dots) <- c("tinytable_latexOptions", class(dots))
  
  return(dots)

}

