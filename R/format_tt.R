
format_vector_sprintf <- function(vec, sprintf_pattern = NULL) {
  if (is.null(sprintf_pattern)) return(vec)
  base::sprintf(sprintf_pattern, vec)
}

format_vector_logical <- function(vec, bool_fn = NULL) {
  if (!is.logical(vec) || is.null(bool_fn)) return(vec)
  bool_fn(vec)
}

format_vector_date <- function(vec, date_format = NULL) {
  if (!inherits(vec, "Date") || is.null(date_format)) return(vec)
  format(vec, date_format)
}

format_vector_other <- function(vec, other_fn = NULL) {
  if (!is.function(other_fn)) return(vec)
  other_fn(vec)
}

format_vector_custom <- function(vec, fn = NULL) {
  if (!is.function(fn)) return(vec)
  fn(vec)
}

format_vector_math <- function(vec, math = FALSE) {
  if (!isTRUE(math)) return(vec)
  sprintf("$%s$", vec)
}

format_vector_replace <- function(ori_vec = NULL, out_vec = NULL, replace = NULL, ...) {
  # Handle case where only one vector is provided (for components like groupi)
  if (is.null(out_vec) && !is.null(ori_vec)) {
    out_vec <- ori_vec
  }
  
  # Handle single argument call from components (like apply_groups_i)
  dots <- list(...)
  if (is.null(replace) && "replace" %in% names(dots)) {
    replace <- dots$replace
  }
  
  # Handle case where out_vec is actually the replace argument (single argument call)
  if (is.null(replace) && is.list(out_vec)) {
    replace <- out_vec
    out_vec <- ori_vec
  }
  
  # If no replace argument, return input unchanged
  if (is.null(replace) || length(replace) == 0) {
    return(ori_vec)
  }
  
  result <- out_vec
  for (k in seq_along(replace)) {
    # Check if this is value replacement (like NA -> "-") or named replacement (like "Down" -> "↓")
    replacement_values <- replace[[k]]
    is_value_replacement <- any(is.na(replacement_values)) || 
                           any(sapply(replacement_values, function(x) identical(x, NaN))) ||
                           any(is.infinite(replacement_values))
    
    if (is_value_replacement) {
      # Value replacement: replace the value (NA/NaN/Inf/-Inf) with the name
      idx <- ori_vec %in% replacement_values | (is.na(ori_vec) & any(is.na(replacement_values)))
      if (identical(names(replace)[[k]], " ")) {
        result[idx] <- ""
      } else {
        result[idx] <- names(replace)[[k]]
      }
    } else {
      # Named replacement: replace the name with the value  
      idx <- ori_vec %in% names(replace)[[k]]
      if (identical(replacement_values, " ")) {
        result[idx] <- ""
      } else {
        result[idx] <- replacement_values
      }
    }
  }
  
  return(result)
}

format_vector_markdown <- function(vec, output_format, markdown = FALSE) {
  if (isFALSE(markdown)) return(vec)
  if (is.null(output_format)) return(vec)
  
  if (output_format == "html") {
    vapply(vec, function(k) {
      k <- litedown::mark(I(k), "html")
      k <- sub("<p>", "", k, fixed = TRUE)
      k <- sub("</p>", "", k, fixed = TRUE)
      return(k)
    }, character(1))
  } else if (output_format == "latex") {
    vapply(vec, function(k) {
      litedown::mark(I(k), "latex")
    }, character(1))
  } else {
    vec
  }
}

format_quarto <- function(out, i, col, x) {
  if (isTRUE(x@output == "html")) {
    fun <- function(z) {
      z@table_string <- sub(
        "data-quarto-disable-processing='true'",
        "data-quarto-disable-processing='false'",
        z@table_string,
        fixed = TRUE
      )
      return(z)
    }
    x <- style_tt(x, finalize = fun)
    out[i, col] <- sprintf(
      '<span data-qmd="%s"></span>',
      out[i, col, drop = TRUE]
    )
  } else if (isTRUE(x@output == "latex")) {
    assert_dependency("base64enc")
    tmp <- sapply(
      out[i, col, drop = TRUE],
      function(z) base64enc::base64encode(charToRaw(z))
    )
    out[i, col] <- sprintf("\\QuartoMarkdownBase64{%s}", tmp)
  }

  return(list("out" = out, "x" = x))
}

# Apply functions for different table elements
apply_cells <- function(out, i, j, format_fn, ...) {
  for (row in i) {
    for (col in j) {
      out[row, col] <- format_fn(out[row, col], ...)
    }
  }
  return(out)
}

apply_caption <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) return(x)
  x@caption <- format_fn(x@caption, ...)
  return(x)
}

apply_notes <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) return(x)
  
  for (idx in seq_along(x@notes)) {
    n <- x@notes[[idx]]
    if (is.character(n) && length(n) == 1) {
      x@notes[[idx]] <- format_fn(n, ...)
    } else if (is.list(n) && "text" %in% names(n)) {
      n$text <- format_fn(n$text, ...)
      x@notes[[idx]] <- n
    }
  }
  return(x)
}

apply_groups_i <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) return(x)
  
  for (idx in seq_along(x@lazy_group)) {
    g <- x@lazy_group[[idx]]
    if (!is.null(g$i)) {
      result <- format_fn(names(g$i), ...)
      if (!is.null(result)) {
        names(g$i) <- result
      }
    }
    x@lazy_group[[idx]] <- g
  }
  return(x)
}

apply_groups_j <- function(x, format_fn, ...) {
  if (!inherits(x, "tinytable")) return(x)
  
  for (idx in seq_along(x@lazy_group)) {
    g <- x@lazy_group[[idx]]
    if (!is.null(g$j)) {
      names(g$j) <- format_fn(names(g$j), ...)
    }
    x@lazy_group[[idx]] <- g
  }
  return(x)
}

apply_colnames <- function(x, format_fn, ...) {
  colnames(x) <- format_fn(colnames(x), ...)
  return(x)
}

# Global dispatcher function
apply_format <- function(out, 
                         x, 
                         i, 
                         j, 
                         format_fn, 
                         ori = NULL, 
                         source = "out", 
                         components = NULL, 
                         inherits = NULL,
                         ...) {

  if (is.character(components)) {
    if ("all" %in% components) {
      components <- c("colnames", "caption", "notes", "groupi", "groupj")
    } else if (!("cells" %in% components)) {
      i <- NULL
      j <- NULL
    }
  }

  # Apply formatting to specified components only
  if ("colnames" %in% components) {
    x <- apply_colnames(x, format_fn, ...)
  }
  if ("caption" %in% components) {
    x <- apply_caption(x, format_fn, ...)
  }
  if ("notes" %in% components) {
    x <- apply_notes(x, format_fn, ...)
  }
  if ("groupi" %in% components) {
    x <- apply_groups_i(x, format_fn, ...)
  }
  if ("groupj" %in% components) {
    x <- apply_groups_j(x, format_fn, ...)
  }

  # Apply to specific cells
  # Filter columns based on inherits argument
  j_filtered <- j
  if (is.character(inherits)) {
    if (source == "ori" && !is.null(ori)) {
      j_filtered <- j[sapply(j, function(col) inherits(ori[[col]], inherits))]
    } else {
      j_filtered <- j[sapply(j, function(col) inherits(out[[col]], inherits))]
    }
  } else if (is.function(inherits)) {
    if (source == "ori" && !is.null(ori)) {
      j_filtered <- j[sapply(j, function(col) inherits(ori[[col]]))]
    } else {
      j_filtered <- j[sapply(j, function(col) inherits(out[[col]]))]
    }
  }

  if (length(j_filtered) > 0) {
    if (source == "both" && !is.null(ori)) {
      # Functions that need both ori and out values
      for (col in j_filtered) {
        tmp <- format_fn(ori[i, col, drop = TRUE], out[i, col, drop = TRUE], ...)
        if (!is.null(tmp)) out[i, col] <- tmp
      }
    } else if (source == "ori" && !is.null(ori)) {
      # Functions that use original values
      for (col in j_filtered) {
        tmp <- format_fn(ori[i, col, drop = TRUE], ...)
        if (!is.null(tmp)) out[i, col] <- tmp
      }
    } else {
      # Functions that use current out values
      if (!is.null(inherits)) {
        # Use custom apply_cells with filtering
        for (row in i) {
          for (col in j_filtered) {
            tmp <- format_fn(out[row, col], ...)
            if (!is.null(tmp)) out[row, col] <- tmp
          }
        }
      } else {
        out <- apply_cells(out, i, j, format_fn, ...)
      }
    }
  }
  
  return(list(out = out, x = x))
}



#' Format columns of a data frame
#'
#' This function formats the columns of a data frame based on the column type (logical, date, numeric).
#' It allows various formatting options like significant digits, decimal points, and scientific notation.
#' It also includes custom formatting for date and boolean values.
#' If this function is applied several times to the same cell, the last transformation is retained and the previous calls are ignored, except for the `escape` argument which can be applied to previously transformed data.
#'
#' @param x A data frame or a vector to be formatted.
#' @param i Row indices where the formatting should be applied. Can also be a character vector specifying table components to format: "all", "cells", "colnames", "caption", "notes", "groupi" (row group labels), "groupj" (column group labels). If both `i` and `j` are omitted (NULL), formatting is applied to all table elements: data cells, column names, caption, notes, and group labels.
#' @param digits Number of significant digits or decimal places.
#' @param num_fmt The format for numeric values; one of 'significant', 'significant_cell', 'decimal', or 'scientific'.
#' @param num_zero Logical; if TRUE, trailing zeros are kept in "decimal" format (but not in "significant" format).
#' @param num_mark_big Character to use as a thousands separator.
#' @param num_mark_dec Decimal mark character. Default is the global option 'OutDec'.
#' @param num_suffix Logical; if TRUE display short numbers with `digits` significant digits and K (thousands), M (millions), B (billions), or T (trillions) suffixes.
#' @param date A string passed to the `format()` function, such as "%Y-%m-%d". See the "Details" section in `?strptime`
#' @param bool A function to format logical columns. Defaults to title case.
#' @param math Logical. If TRUE, wrap cell values in math mode `$..$`. This is useful for LaTeX output or with HTML MathJax `options(tinytable_html_mathjax=TRUE)`.
#' @param other A function to format columns of other types. Defaults to `as.character()`.
#' @param replace Logical, String or Named list of vectors
#' - TRUE: Replace `NA` and `NaN` by an empty string.
#' - FALSE: Print `NA` and `NaN` as strings.
#' - String: Replace `NA` and `NaN` entries by the user-supplied string.
#' - Named list: Replace matching elements of the vectors in the list by theirs names. Example:
#'      - `list("-" = c(NA, NaN), "Tiny" = -Inf, "Massive" = Inf)`
#' @param escape Logical or "latex" or "html". If TRUE, escape special characters to display them as text in the format of the output of a `tt()` table.
#' - If `i` and `j` are both `NULL`, escape all cells, column names, caption, notes, and spanning labels created by `group_tt()`.
#' @param markdown Logical; if TRUE, render markdown syntax in cells. Ex: `_italicized text_` is properly italicized in HTML and LaTeX.
#' @param fn Function for custom formatting. Accepts a vector and returns a character vector of the same length.
#' @param quarto Logical. Enable Quarto data processing and wrap cell content in a `data-qmd` span (HTML) or `\QuartoMarkdownBase64{}` macro (LaTeX). See warnings in the Global Options section below.
#' @param sprintf String passed to the `?sprintf` function to format numbers or interpolate strings with a user-defined pattern (similar to the `glue` package, but using Base R).
#' @inheritParams tt
#' @inheritParams style_tt
#' @template global_options
#'
#' @return A data frame with formatted columns.
#' @export
#' @examples
#' dat <- data.frame(
#'   a = rnorm(3, mean = 10000),
#'   b = rnorm(3, 10000)
#' )
#' tab <- tt(dat)
#' format_tt(tab,
#'   digits = 2,
#'   num_mark_dec = ",",
#'   num_mark_big = " "
#' )
#'
#' k <- tt(data.frame(x = c(0.000123456789, 12.4356789)))
#' format_tt(k, digits = 2, num_fmt = "significant_cell")
#'
#' dat <- data.frame(
#'   a = c("Burger", "Halloumi", "Tofu", "Beans"),
#'   b = c(1.43202, 201.399, 0.146188, 0.0031),
#'   c = c(98938272783457, 7288839482, 29111727, 93945)
#' )
#' tt(dat) |>
#'   format_tt(j = "a", sprintf = "Food: %s") |>
#'   format_tt(j = 2, digits = 1, num_fmt = "decimal", num_zero = TRUE) |>
#'   format_tt(j = "c", digits = 2, num_suffix = TRUE)
#'
#' y <- tt(data.frame(x = c(123456789.678, 12435.6789)))
#' format_tt(y, digits = 3, num_mark_big = " ")
#'
#' x <- tt(data.frame(Text = c("_italicized text_", "__bold text__")))
#' format_tt(x, markdown = TRUE)
#'
#' tab <- data.frame(a = c(NA, 1, 2), b = c(3, NA, 5))
#' tt(tab) |> format_tt(replace = "-")
#'
#' dat <- data.frame(
#'   "LaTeX" = c("Dollars $", "Percent %", "Underscore _"),
#'   "HTML" = c("<br>", "<sup>4</sup>", "<emph>blah</emph>")
#' )
#' tt(dat) |> format_tt(escape = TRUE)
#'
format_tt <- function(
  x,
  i = NULL,
  j = NULL,
  digits = get_option("tinytable_format_digits", default = NULL),
  num_fmt = get_option("tinytable_format_num_fmt", default = "significant"),
  num_zero = get_option("tinytable_format_num_zero", default = FALSE),
  num_suffix = get_option("tinytable_format_num_suffix", default = FALSE),
  num_mark_big = get_option("tinytable_format_num_mark_big", default = ""),
  num_mark_dec = get_option(
    "tinytable_format_num_mark_dec",
    default = getOption("OutDec", default = ".")
  ),
  date = get_option("tinytable_format_date", default = NULL),
  bool = get_option("tinytable_format_bool", default = NULL),
  math = get_option("tinytable_format_math", default = FALSE),
  other = get_option("tinytable_format_other", default = NULL),
  replace = get_option("tinytable_format_replace", default = FALSE),
  escape = get_option("tinytable_format_escape", default = FALSE),
  markdown = get_option("tinytable_format_markdown", default = FALSE),
  quarto = get_option("tinytable_format_quarto", default = FALSE),
  fn = get_option("tinytable_format_fn", default = NULL),
  sprintf = get_option("tinytable_format_sprintf", default = NULL)
) {
  assert_integerish(digits, len = 1, null.ok = TRUE)
  assert_choice(
    num_fmt,
    c("significant", "significant_cell", "decimal", "scientific")
  )
  assert_flag(num_zero)
  assert_string(num_mark_big)
  assert_string(num_mark_dec)
  assert_string(date, null.ok = TRUE)
  assert_function(bool, null.ok = TRUE)
  assert_function(identity, null.ok = TRUE)
  assert_function(other, null.ok = TRUE)
  assert_flag(markdown)
  assert_flag(quarto)
  assert_function(fn, null.ok = TRUE)
  assert_string(sprintf, null.ok = TRUE)
  replace <- sanitize_replace(replace)
  sanity_num_mark(digits, num_mark_big, num_mark_dec)

  # Check if i contains component names (do this before processing tinytable objects)
  if (is.character(i)) {
    components <- i
    i <- NULL
  } else if (!is.null(i) || !is.null(j)) {
    components <- "cells"
  } else {
    components <- "all"
  }

  out <- x

  if (inherits(out, "tinytable")) {
    cal <- call(
      "format_tt_lazy",
      i = i,
      j = j,
      digits = digits,
      num_fmt = num_fmt,
      num_zero = num_zero,
      num_suffix = num_suffix,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      replace = replace,
      fn = fn,
      sprintf = sprintf,
      date = date,
      bool = bool,
      math = math,
      escape = escape,
      markdown = markdown,
      quarto = quarto,
      other = other,
      components = components
    )
    out@lazy_format <- c(out@lazy_format, list(cal))
  } else {
    out <- format_tt_lazy(
      out,
      i = i,
      j = j,
      digits = digits,
      num_fmt = num_fmt,
      num_zero = num_zero,
      num_suffix = num_suffix,
      num_mark_big = num_mark_big,
      num_mark_dec = num_mark_dec,
      replace = replace,
      fn = fn,
      sprintf = sprintf,
      date = date,
      bool = bool,
      math = math,
      other = other,
      escape = escape,
      quarto = quarto,
      markdown = markdown,
      components = components
    )
  }

  return(out)
}


format_tt_lazy <- function(
  x,
  i,
  j,
  digits,
  num_fmt,
  num_zero,
  num_suffix,
  num_mark_big,
  num_mark_dec,
  replace,
  fn,
  sprintf,
  date,
  bool,
  math,
  escape,
  markdown,
  quarto,
  other,
  components = NULL
) {
  if (inherits(x, "tbl_df")) {
    assert_dependency("tibble")
    x_is_tibble <- TRUE
    x <- as.data.frame(x, check.names = FALSE)
  } else {
    x_is_tibble <- FALSE
  }

  # format_tt() supports vectors
  if (isTRUE(check_atomic_vector(x))) {
    atomic_vector <- TRUE
    if (is.factor(x)) {
      x <- as.character(x)
    }
    ori <- out <- x <- data.frame(tinytable = x, stringsAsFactors = FALSE)
    j <- 1
  } else if (is.data.frame(x)) {
    atomic_vector <- FALSE
    ori <- out <- x
  } else if (inherits(x, "tinytable")) {
    atomic_vector <- FALSE
    # if no other format_tt() call has been applied, we ctan have numeric values
    out <- x@table_dataframe
    ori <- x@data
  } else {
    stop(
      "`x` must be a `tinytable` object, a data frame, or an atomic vector.",
      call. = FALSE
    )
  }


  # In sanity_tt(), we fill in missing NULL `j` in the format-specific versions,
  # because tabularray can do whole column styling. Here, we need to fill in
  # NULL for all formats since this is applied before creating the table.
  # nrow(out) because nrow(x) sometimes includes rows that will be added **in the lazy future** by group_tt()
  i <- sanitize_i(i, x, lazy = FALSE, calling_function = "format_tt")
  j <- sanitize_j(j, x)
  

  result <- apply_format(out = out, x = x, i = i, j = j, 
    format_fn = format_vector_logical, ori = ori, source = "ori", 
    inherits = "logical", bool_fn = bool)
  out <- result$out
  x <- result$x

  result <- apply_format(out = out, x = x, i = i, j = j, 
    format_fn = format_vector_date, ori = ori, source = "ori", 
    inherits = "Date", date_format = date)
  out <- result$out
  x <- result$x

  if (!is.null(digits)) {
    result <- apply_format(out = out, x = x, i = i, j = j,
      format_fn = format_numeric, ori = ori, source = "ori", num_suffix = num_suffix,
      digits = digits, num_mark_big = num_mark_big, num_mark_dec = num_mark_dec, 
      num_zero = num_zero, num_fmt = num_fmt, inherits = is.numeric)
    out <- result$out
    x <- result$x
  }

  is_other <- function(x) !is.numeric(x) && !inherits(x, "Date") && !is.logical(x)
  result <- apply_format(out = out, x = x, i = i, j = j,
    format_fn = format_vector_other, ori = ori, source = "ori", inherits = is_other, 
    other_fn = other)
  out <- result$out
  x <- result$x

  # format each column using the original approach
  # Issue #230: drop=TRUE fixes bug which returned a character dput-like vector
  result <- apply_format(out = out, ori = ori, x = x, i = i, j = j, 
    format_fn = format_vector_replace, 
    source = "both", replace = replace, components = components)
  out <- result$out
  x <- result$x

  # after other formatting
  if (!is.null(sprintf)) {
    result <- apply_format(out = out, x = x, i = i, j = j, 
      format_fn = format_vector_sprintf, ori = ori, source = "ori", sprintf_pattern = sprintf)
    out <- result$out
    x <- result$x
  }

  # Custom functions overwrite all the other formatting, but is before markdown
  # before escaping
  if (is.function(fn)) {
    if (!is.null(components)) {
      # Use apply_format for component-specific formatting
      result <- apply_format(out = out, x = x, i = i, j = j, format_fn = format_vector_custom, ori = ori, source = "ori", components = components, fn = fn)
      out <- result$out
      x <- result$x
    } else {
      # Original behavior for cell-specific formatting
      for (col in j) {
        out[i, col] <- format_vector_custom(ori[i, col, drop = TRUE], fn)
      }
    }
  }

  # close to last
  if (isTRUE(math)) {
    result <- apply_format(out = out, x = x, i = i, j = j, format_fn = format_vector_math, components = components, math = math)
    out <- result$out
    x <- result$x
  }

  # escape latex characters
  if (!isFALSE(escape)) {
    if (isTRUE(escape == "latex")) {
      o <- "latex"
    } else if (isTRUE(escape == "html")) {
      o <- "html"
    } else if (isTRUE(escape == "typst")) {
      o <- "typst"
    } else if (inherits(x, "tinytable")) {
      o <- x@output
    } else {
      o <- FALSE
    }

    result <- apply_format(out = out, x = x, i = i, j = j, format_fn = escape_text, components = components, output = o)
    out <- result$out
    x <- result$x
    
    # column names when 0 is in i
    if (0 %in% i && !identical(components, "all")) {
      x <- apply_colnames(x, escape_text, output = o)
    }
  }

  # markdown and quarto at the very end
  if (isTRUE(markdown)) assert_dependency("litedown")
  result <- apply_format(out = out, x = x, i = i, j = j, format_fn = format_vector_markdown, components = components, output_format = x@output, markdown = markdown)
  out <- result$out
  x <- result$x

  if (isTRUE(quarto)) {
    for (col in j) {
      tmp <- format_quarto(out = out, i = i, col = col, x = x)
      out <- tmp$out
      x <- tmp$x
    }
  }

  # output
  if (isTRUE(atomic_vector)) {
    return(out[[1]])
  } else if (!inherits(x, "tinytable")) {
    if (x_is_tibble) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  } else {
    x@table_dataframe <- out
    return(x)
  }
}



