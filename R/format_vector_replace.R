format_vector_replace <- function(
  vec,
  vec_original = NULL,
  replace = NULL,
  ...
) {
  if (is.null(replace) || isFALSE(replace) || length(replace) == 0) {
    return(vec)
  }

  result <- vec

  # match against the original (pre-formatting) values only: a cell that
  # merely *formats* to the same string as a replacement target (e.g. 1.4
  # printed as "1" with digits = 0) must not be clobbered
  ori <- if (is.null(vec_original)) vec else vec_original
  ori_chr <- as.character(ori)
  ori_numeric <- is.numeric(ori)

  for (z in seq_along(replace)) {
    new <- names(replace)[z]

    for (old in replace[[z]]) {
      if (is.nan(old)) {
        match_idx <- is.nan(ori)
      } else if (is.na(old)) {
        match_idx <- is.na(ori)
      } else if (ori_numeric && is.numeric(old) && is.infinite(old)) {
        match_idx <- is.infinite(ori) & sign(ori) == sign(old)
      } else {
        match_idx <- !is.na(ori_chr) & ori_chr == as.character(old)
      }

      result[match_idx] <- new
    }
  }

  return(result)
}
