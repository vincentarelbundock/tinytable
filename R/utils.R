# (pseudo-)unique IDs
get_id <- function(stem = "id") {
  id <- sample(c(0:9, letters), 20, replace = TRUE)
  paste0(stem, paste(id, collapse = ""))
}

# getOption with deprecation warnings
get_option <- function(x, default = NULL) {
  deprecated <- c(
    # old = new
    "tinytable_grid_hlines" = "tinytable_markdown_hlines",
    "tinytable_save_pdf_clean" = "tinytable_pdf_clean",
    "tinytable_save_pdf_engine" = "tinytable_pdf_engine"
  )
  if (x %in% names(deprecated)) {
    x_new <- deprecated[x]
    warning(
      sprintf("Option `%s` is deperacated. Use `%s` instead.", x, x_new)
    )
    x <- x_new
  }
  getOption(x, default = default)
}

format_markup_num <- function(x) {
  out <- format(
    x,
    scientific = FALSE,
    trim = TRUE,
    digits = 15,
    decimal.mark = ".",
    drop0trailing = TRUE
  )
  sub("^-0$", "0", out)
}

format_markup_unit <- function(x, unit) {
  paste0(format_markup_num(x), unit)
}

ttempdir <- function() {
  d <- tempdir()
  d <- file.path(d, "tinytable")
  # start fresh
  if (dir.exists(d)) {
    unlink(d, recursive = TRUE)
  }
  dir.create(d)
  return(d)
}

# Line helpers operate in two layers:
#   - `*_vec()` take and return a character vector of lines (no splitting /
#     collapsing). These are the cheap primitives meant to be reused many
#     times while a table string is held in its split form.
#   - the original string-based `lines_*()` are thin wrappers that split once,
#     delegate to the `*_vec()` primitive and collapse back once. Behaviour is
#     byte-for-byte identical to the previous inline implementations.
#
# Holding a table as a character vector across a whole build phase (see
# `table_string_lines<-`) lets the hot backends call the `*_vec()` primitives
# in a loop without re-`strsplit()`-ing and re-`paste()`-ing the *entire,
# growing* table string on every single insertion. See tt_save_audit.md §4.2.

lines_drop_consecutive_empty_vec <- function(lines) {
  tmp <- rle(lines)
  tmp$lengths[trimws(tmp$values) == ""] <- 1
  inverse.rle(tmp)
}

lines_drop_consecutive_empty <- function(x) {
  lines <- strsplit(x, "\n")[[1]]
  lines <- lines_drop_consecutive_empty_vec(lines)
  paste0(lines, collapse = "\n")
}

lines_drop_vec <- function(
    lines,
    regex,
    position = "equal",
    fixed = FALSE,
    unique = TRUE,
    perl = FALSE) {
  assert_choice(position, c("equal", "before", "after", "all"))
  idx <- grep(regex, lines, fixed = fixed, perl = perl)
  if (isTRUE(unique) && length(idx) > 1 && position != "all") {
    stop(
      "The `regex` supplied `lines_drop()` did not match a unique line.",
      call. = FALSE
    )
  }
  if (!anyNA(idx)) {
    if (position == "equal") {
      lines <- lines[!seq_along(lines) %in% idx]
    } else if (position == "before") {
      lines <- lines[idx:length(lines)]
    } else if (position == "after") {
      lines <- lines[1:idx]
    } else if (position == "all") {
      lines <- lines[!seq_along(lines) %in% idx]
    }
  }
  lines
}

lines_drop <- function(
    old,
    regex,
    position = "equal",
    fixed = FALSE,
    unique = TRUE,
    perl = FALSE) {
  lines <- strsplit(old, "\n")[[1]]
  lines <- lines_drop_vec(
    lines, regex, position = position, fixed = fixed, unique = unique, perl = perl
  )
  paste(lines, collapse = "\n")
}

lines_drop_between_vec <- function(lines, regex_start, regex_end, fixed = FALSE, perl = FALSE) {
  idx_start <- grep(regex_start, lines, fixed = fixed, perl = perl)
  idx_end <- grep(regex_end, lines, fixed = fixed, perl = perl)
  if (length(idx_start) != 1) {
    stop("The `regex_start` did not match a unique line.", call. = FALSE)
  }
  if (length(idx_end) != 1) {
    stop("The `regex_end` did not match a unique line.", call. = FALSE)
  }
  if (idx_start >= idx_end) {
    stop("`regex_start` matches a line after `regex_end`.", call. = FALSE)
  }
  keep <- c(1:(idx_start - 1), (idx_end + 1):length(lines))
  lines[keep]
}

lines_drop_between <- function(text, regex_start, regex_end, fixed = FALSE, perl = FALSE) {
  lines <- strsplit(text, "\n")[[1]]
  lines <- lines_drop_between_vec(lines, regex_start, regex_end, fixed = fixed, perl = perl)
  paste(lines, collapse = "\n")
}

lines_insert_vec <- function(lines, new, regex, position = c("before", "after"),
                             fixed = FALSE, perl = FALSE, occurrence = 1L,
                             require_unique = FALSE) {
  position <- match.arg(position)
  hits <- grep(regex, lines, fixed = fixed, perl = perl)

  if (length(hits) == 0L || anyNA(hits)) stop("`regex` did not match.", call. = FALSE)
  if (require_unique && length(hits) != 1L) stop("`regex` did not match a unique line.", call. = FALSE)
  if (occurrence > length(hits)) stop("`occurrence` out of range.", call. = FALSE)

  idx <- hits[occurrence]
  after <- if (position == "before") idx - 1L else idx
  # Split `new` into its constituent lines so that the returned vector always
  # mirrors the fully-split state that the string-based helpers would reach
  # after a collapse + re-split. This is what keeps a chain of `*_vec()` calls
  # byte-for-byte identical to the equivalent chain of string round-trips:
  # multi-line inserts must NOT stay lumped into a single element, otherwise a
  # later marker lookup could match the whole block instead of a single line
  # (e.g. the Typst footer template embeds its own "// tinytable notes after"
  # marker). strsplit("") yields character(0); preserve the original
  # empty-insert behaviour by mapping that back to a single blank line.
  new <- unlist(strsplit(new, "\n", fixed = TRUE), use.names = FALSE)
  if (length(new) == 0L) new <- ""
  append(lines, values = new, after = after)
}

lines_insert <- function(old, new, regex, position = c("before", "after"),
                         fixed = FALSE, perl = FALSE, occurrence = 1L,
                         require_unique = FALSE) {
  lines <- strsplit(old, "\n", fixed = TRUE)[[1]]
  lines <- lines_insert_vec(
    lines, new, regex, position = position, fixed = fixed, perl = perl,
    occurrence = occurrence, require_unique = require_unique
  )
  paste(lines, collapse = "\n")
}


rbind_nocol <- function(...) {
  dflist <- list(...)
  out <- lapply(list(...), function(k) stats::setNames(k, seq_len(ncol(k))))
  do.call(rbind, out)
}


...get <- function(x, ifnotfound = NULL) {
  eval(
    quote(
      if (!anyNA(.m1 <- match(.x, ...names())) && !is.null(.m2 <- ...elt(.m1))) {
        .m2
      } else {
        .ifnotfound
      }),
    pairlist(.x = x[1L], .ifnotfound = ifnotfound),
    parent.frame(1L)
  )
}


#' Helper function to get value with fallback
#' @param value Primary value
#' @param fallback Fallback value
#' @return Value or fallback
#' @keywords internal
#' @noRd
`%||%` <- function(value, fallback) {
  if (is.null(value)) fallback else value
}


percent_sum_100 <- function(x, digits = 0) {
  stopifnot(is.numeric(x), all(is.finite(x)), all(x >= 0))
  s <- sum(x)
  if (s <= 0) stop("sum(x) must be > 0")
  p <- x / s

  units <- 100L * 10^digits
  scaled <- p * units
  base <- floor(scaled)
  rem <- scaled - base

  leftover <- units - sum(base) # how many 1/10^digits to distribute
  if (leftover != 0) {
    ix <- order(rem, decreasing = TRUE, method = "radix")
    base[ix[seq_len(leftover)]] <- base[ix[seq_len(leftover)]] + 1L
  }

  out <- base / 10^digits
  return(out)
}
