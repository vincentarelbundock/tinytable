#' LaTeX options
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
  assert_choice(theme, c("booktabs", "plain"))

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

  if ("hlines" %in% names(dots)) {
    dots$hlines <- sprintf("{%s}", dots$hlines)
  }

  if ("vlines" %in% names(dots)) {
    dots$vlines <- sprintf("{%s}", dots$vlines)
  }

  # clean dots
  dots <- dots[dots != ""]
  dots <- sapply(names(dots), function(n) paste0(n, "=", dots[[n]]))

  out <- list(
    rows_keys = c("halign", "valign", "ht", "bg", "fg", "font", "mode", "cmd", "abovesep", "belowsep", "rowsep", "preto", "appto"),
    columns_keys = c("halign", "valign", "wd", "co", "bg", "fg", "font", "mode", "cmd", "leftsep", "rightsep", "colsep", "preto", "appto"),
    hborders_keys = c("pagebreak", "abovespace", "belowspace"),
    vborders_keys = c("leftspace", "rightspace"),
    cells_keys = c("halign", "valign", "wd", "bg", "fg", "font", "mode", "cmd"),
    outer_specs_keys = c("baseline", "long", "tall", "expand"),
    inner_specs_keys = c("rulesep", "hlines", "vlines", "stretch", "abovesep", "belowsep", "rowsep", "leftsep", "rightsep", "colsep", "hspan", "vspan", "baseline"),
    span = c("r", "c")
  )
  out <- lapply(out, function(x) intersect(x, names(dots)))
  out <- lapply(out, function(x) dots[x])
  out <- sapply(out, paste, collapse = ",")

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

