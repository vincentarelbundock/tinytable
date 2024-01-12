group_bootstrap <- function(x, i, italic = TRUE, rule = TRUE, indent = 1) {
  assert_integerish(i)
  assert_flag(rule)
  assert_flag(italic)
  if (is.null(names(i))) {
    msg <- "`i` must be a named integer vector."
  }
  label <- names(i)

  # reverse order is important
  i <- rev(sort(i))

  ncol <- attr(x, "ncol")
  att <- attributes(x)
  att$nrow <- att$nrow + length(label)
  tab <- strsplit(x, "\\n")[[1]]
  out <- x

  for (g in seq_along(i)) {
    js <- sprintf(
      "window.addEventListener('load', function () { insertSpanRow(%s, %s, '%s') });",
      i[g],
      attr(x, "ncol"),
      names(i)[g])
    out <- bootstrap_setting(out, new = js, component = "cell")
  }

  # need unique function names in case there are
  # multiple tables in one Rmarkdown document
  out <- gsub(
    "insertSpanRow(",
    paste0("insertSpanRow_", get_id(""), "("),
    out,
    fixed = TRUE)

  idx <- insert_values(seq_len(attr(x, "nrow")), rep(NA, length(i)), i)
  idx_old <- idx$new[!is.na(idx$old)]
  idx_new <- idx$new[is.na(idx$old)]
  out <- style_tt(out, i = idx_old, j = 1, indent = indent)
  if (isTRUE(italic)) {
    out <- style_tt(out, i = idx_new, j = 1, italic = italic)
  }

  return(out)
}
