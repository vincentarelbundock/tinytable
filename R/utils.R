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

lines_drop_consecutive_empty <- function(x) {
  lines <- strsplit(x, "\n")[[1]]
  tmp <- rle(lines)
  tmp$lengths[trimws(tmp$values) == ""] <- 1
  lines <- inverse.rle(tmp)
  x <- paste0(lines, collapse = "\n")
  return(x)
}

lines_drop <- function(
    old,
    regex,
    position = "equal",
    fixed = FALSE,
    unique = TRUE,
    perl = FALSE) {
  assert_choice(position, c("equal", "before", "after", "all"))
  lines <- strsplit(old, "\n")[[1]]
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
  out <- paste(lines, collapse = "\n")
  return(out)
}

lines_drop_between <- function(text, regex_start, regex_end, fixed = FALSE, perl = FALSE) {
  lines <- strsplit(text, "\n")[[1]]
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
  lines_to_keep <- c(1:(idx_start - 1), (idx_end + 1):length(lines))
  output <- lines[lines_to_keep]
  out <- paste(output, collapse = "\n")
  return(out)
}

lines_insert <- function(old, new, regex, position = c("before", "after"),
                         fixed = FALSE, perl = FALSE, occurrence = 1L,
                         require_unique = FALSE) {
  position <- match.arg(position)
  lines <- strsplit(old, "\n", fixed = TRUE)[[1]]
  hits <- grep(regex, lines, fixed = fixed, perl = perl)

  if (length(hits) == 0L || anyNA(hits)) stop("`regex` did not match.", call. = FALSE)
  if (require_unique && length(hits) != 1L) stop("`regex` did not match a unique line.", call. = FALSE)
  if (occurrence > length(hits)) stop("`occurrence` out of range.", call. = FALSE)

  idx <- hits[occurrence]
  after <- if (position == "before") idx - 1L else idx
  out <- append(lines, values = new, after = after)
  paste(out, collapse = "\n")
}


# strip ANSI from `tibble`/`pillar`; keep for markdown
render_fansi <- function(x) {
  tab <- x@data_body
  if (isTRUE(check_dependency("fansi"))) {
    for (col in seq_along(tab)) {
      if (isTRUE(x@output %in% c("html", "bootstrap", "tabulator"))) {
        tab[[col]] <- as.character(fansi::to_html(tab[[col]], warn = FALSE))
      } else if (isTRUE(!x@output %in% c("markdown", "dataframe"))) {
        tab[[col]] <- as.character(fansi::strip_ctl(tab[[col]]))
      }
    }
  }
  x@data_body <- tab
  return(x)
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
