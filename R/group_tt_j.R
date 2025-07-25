#' Convert delimiter-based column names to named list
#'
#' @keywords internal
#' @noRd
j_delim_to_named_list <- function(x, j) {
  nm <- x@names

  # Find which elements contain the delimiter, and optionally j. Others are left as is
  check_for_multiple_delims <- any(
    lengths(gregexec(pattern = j, text = nm)) > 1L
  )
  if (check_for_multiple_delims) {
    warning(
      "Multiple delimiters found in column names. Only the first delimiter will be used for grouping."
    )
  }

  indices <- grepl(j, nm, fixed = TRUE)
  groupnames <- sub(
    pattern = paste0(j, ".*"),
    replacement = "",
    x = nm[indices]
  )
  indices <- which(grepl(j, nm, fixed = TRUE))
  groupnames <- split(indices, groupnames)

  # Extract suffixes (after delimiter) used as new sub-headers
  colnames <- sub(pattern = paste0(".*?", j), replacement = "", x = nm)

  out <- list(colnames = colnames, groupnames = groupnames)
  return(out)
}

#' Validate and process group indices
#'
#' @keywords internal
#' @noRd
sanitize_group_index <- function(idx, hi, orientation) {
  if (is.null(idx)) {
    return(idx)
  }
  assert_list(idx, named = TRUE)
  for (n in names(idx)) {
    if (orientation == "row") {
      assert_integerish(idx[[n]], len = 1, lower = 1, upper = hi, name = n)
    } else {
      assert_integerish(idx[[n]], lower = 1, upper = hi, name = n)
    }
  }
  # allow duplicated indices for consecutive rows
  # if (anyDuplicated(unlist(idx)) > 0) stop("Duplicate group indices.", call. = FALSE)
  out <- lapply(idx, function(x) min(x):max(x))
  return(out)
}

#' Convert vector to list format for grouping
#'
#' @keywords internal
#' @noRd
sanitize_group_vec2list <- function(vec) {
  if (is.factor(vec)) {
    vec <- as.character(vec)
  }
  rle_result <- rle(vec)
  idx <- cumsum(c(1, utils::head(rle_result$lengths, -1)))
  idx <- as.list(idx)
  names(idx) <- rle_result$values
  return(idx)
}

