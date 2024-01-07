latexOptions <- function(
  env = "table+tblr",
  extendable = FALSE,
  placement = NULL,
  wd = "",
  fg = "",
  bg = "",
  # cells
  halign = "",
  valign = "",
  font = "",
  mode = "",
  cmd = "",
  preto = "",
  appto = "",
  # rows
  ht = "",
  abovesep = "",
  belowsep = "",
  rowsep = "",
  r = 1,
  c = 1,
  # columns
  co = 1,
  leftsep = "",
  rightsep = "",
  colsep = "",
  # hlines
  dash = "",
  text = "",
  leftpos = 1,
  rightpos = 1,
  endpos = TRUE,
  # vlines
  abovepos = 0,
  belowpos = 0
  ) {

  out <- ""

  assert_flag(extendable)

  assert_choice(env, c("table+tblr", "tblr"))
  if (env %in% c("table+tblr", "tbrl")) {
    template <- readLines(system.file("templates/tblr.tex", package = "tinytable"))
  }

  assert_string(placement, null.ok = TRUE)
  if (is.null(placement)) {
    placement <- ""
  } else {
    placement <- sprintf("[%s]", placement)
  }
  template <- sub("$TINYTABLE_PLACEMENT", placement, template)

  arguments <- list()

  args <- list(wd = wd, fg = fg, bg = bg, halign = halign, valign = valign, font = font, cmd = cmd, preto = preto, appto = appto, ht = ht, abovesep = abovesep, belowsep = belowsep, rowsep = rowsep, leftsep = leftsep, rightsep = rightsep, dash = dash, text = text)
  for (n in names(args)) {
    assert_string(args[[n]], name = n)
  }
  arguments <- c(arguments, args)

  assert_choice(mode, c("", "math", "imath", "text"))
  arguments <- c(arguments, list(mode = mode))

  assert_flag(endpos)
  if (!isTRUE(endpos)) {
    arguments <- c(arguments, list(endpos = "false"))
  }
  
  args <- list(c = c, r = r, co = co, leftpos = leftpos, rightpos = rightpos)
  for (n in names(args)) {
    assert_integerish(args[[n]], len = 1, name = n)
    if (args[[n]] == 1) args[[n]] <- NULL
  }
  arguments <- c(arguments, args)
 
  args <- list(abovepos = abovepos, belowpos = belowpos)
  for (n in names(args)) {
    assert_integerish(args[[n]], len = 1, name = n)
    if (args[[n]] == 0) args[[n]] <- NULL
  }
  arguments <- c(arguments, args)

  out <- list(
    rows_keys = c("halign", "valign", "ht", "bg", "fg", "font", "mode", "cmd", "abovesep", "belowsep", "rowsep", "preto", "appto"),
    columns_keys = c("halign", "valign", "wd", "co", "bg", "fg", "font", "mode", "cmd", "leftsep", "rightsep", "colsep", "preto", "appto"),
    hborders_keys = c("pagebreak", "abovespace", "belowspace"),
    vborders_keys = c("leftspace", "rightspace"),
    cells_keys = c("halign", "valign", "wd", "bg", "fg", "font", "mode", "cmd"),
    outer_specs_keys = c("baseline", "long", "tall", "expand"),
    inner_specs_keys = c("rulesep", "stretch", "abovesep", "belowsep", "rowsep", "leftsep", "rightsep", "colsep", "hspan", "vspan", "baseline")
  )
  out <- lapply(out, function(x) intersect(x, names(arguments)))

  arguments <- list(
    extendable = extendable,
    template = template,
    spec = arguments
  )
  class(arguments) <- c("tinytable_latexOptions", class(arguments))
  
  return(arguments)

}

