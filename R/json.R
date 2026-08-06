#' Mark a string as raw JSON/JavaScript
#'
#' Values wrapped with `json_raw()` are emitted verbatim (unquoted, unescaped)
#' by `df_to_json()`. Use this for pre-serialized JSON fragments (e.g. sparkline
#' data arrays) or JavaScript identifiers that must not be quoted.
#'
#' @param x Character string containing raw JSON or JavaScript
#' @return `x` with class `tt_json_raw`
#' @keywords internal
#' @noRd
json_raw <- function(x) structure(x, class = "tt_json_raw")


#' Escape a character vector for use inside a JSON string literal
#'
#' @param s Character vector
#' @return Escaped character vector
#' @keywords internal
#' @noRd
json_escape_string <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub('"', '\\"', s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  # JS-breaking line separators
  s <- gsub("\u2028", "\\u2028", s, fixed = TRUE)
  s <- gsub("\u2029", "\\u2029", s, fixed = TRUE)
  s
}


# Convert a single value to JSON
value_to_json <- function(value) {
  if (inherits(value, "tt_json_raw")) {
    return(unclass(value))
  }
  if (is.null(value)) {
    return("null")
  }
  if (length(value) == 1 && !is.list(value) && is.na(value)) {
    return("null")
  }
  if (is.numeric(value) && length(value) == 1) {
    if (!is.finite(value)) {
      return("null") # bare Inf/-Inf is a JS ReferenceError
    }
    return(as.character(value)) # Return as unquoted number for JSON
  }
  if (is.logical(value) && length(value) == 1) {
    return(if (value) "true" else "false")
  }
  if (is.list(value)) {
    # Recursively handle nested lists
    return(list_to_json(value))
  }
  if (length(value) == 1) {
    # Single string value: always quoted and escaped
    return(paste0('"', json_escape_string(as.character(value)), '"'))
  }
  # Vector - convert to array
  json_values <- vapply(value, function(v) {
    if (is.na(v)) {
      "null"
    } else if (is.numeric(v)) {
      if (is.finite(v)) as.character(v) else "null"
    } else if (is.logical(v)) {
      if (v) "true" else "false"
    } else {
      paste0('"', json_escape_string(as.character(v)), '"')
    }
  }, character(1))
  paste0("[", paste(json_values, collapse = ","), "]")
}


# Convert a named list to a JSON object
list_to_json <- function(lst) {
  if (length(lst) == 0) {
    return("{}")
  }

  pairs <- sapply(names(lst), function(name) {
    # Special handling for formatter and sorter fields - don't quote custom function names
    if ((name == "formatter" || name == "sorter") && is.character(lst[[name]]) &&
        grepl("^tinytable_", lst[[name]])) {
      # Custom formatter/sorter - output as unquoted function reference
      paste0('"', json_escape_string(name), '":', lst[[name]])
    } else {
      value_json <- value_to_json(lst[[name]])
      paste0('"', json_escape_string(name), '":', value_json)
    }
  })

  paste0("{", paste(pairs, collapse = ","), "}")
}


# Serialize one data frame column to a character vector of JSON values
json_serialize_column <- function(col) {
  if (is.list(col)) {
    # List-columns (e.g. raw JSON sparkline cells) go through value_to_json
    return(vapply(col, value_to_json, character(1)))
  }
  if (is.numeric(col)) {
    out <- as.character(col)
    out[!is.finite(col)] <- "null" # NA/NaN/Inf/-Inf -> null
    return(out)
  }
  if (is.logical(col)) {
    out <- ifelse(col, "true", "false")
    out[is.na(col)] <- "null"
    return(out)
  }
  # character, factor, Date, etc.
  s <- as.character(col)
  out <- paste0('"', json_escape_string(s), '"')
  out[is.na(s)] <- "null"
  out
}


#' Convert data frame or list to JSON
#'
#' Simple helper function to convert a data frame or list to JSON format
#'
#' @param data A data frame or list to convert to JSON
#'
#' @return JSON string
#' @keywords internal
df_to_json <- function(data) {
  # Handle list input (for column definitions)
  if (is.list(data) && !is.data.frame(data)) {
    # Check if this is a list of lists (array of objects)
    if (length(data) > 0 && all(sapply(data, is.list))) {
      # Convert list of lists to JSON array
      json_objects <- sapply(data, list_to_json)
      return(paste0("[", paste(json_objects, collapse = ","), "]"))
    } else {
      # Single object
      return(list_to_json(data))
    }
  }

  # Handle data frame input - convert to array of objects (column-wise, vectorized)
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(paste0("[", paste(rep("{}", nrow(data)), collapse = ","), "]"))
  }

  keys <- paste0('"', json_escape_string(names(data)), '":')
  cols <- lapply(data, json_serialize_column)
  pieces <- Map(function(k, v) paste0(k, v), keys, cols)
  rows <- do.call(paste, c(unname(pieces), list(sep = ",")))
  paste0("[", paste0("{", rows, "}", collapse = ","), "]")
}
