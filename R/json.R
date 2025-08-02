#' Convert data frame or list to JSON
#'
#' Simple helper function to convert a data frame or list to JSON format
#'
#' @param data A data frame or list to convert to JSON
#'
#' @return JSON string
#' @keywords internal
df_to_json <- function(data) {
  # Helper function to convert a value to JSON
  value_to_json <- function(value) {
    if (is.null(value)) {
      return("null")
    } else if (length(value) == 1 && is.na(value)) {
      return("null")
    } else if (is.numeric(value) && length(value) == 1) {
      return(as.character(value)) # Return as unquoted number for JSON
    } else if (is.logical(value) && length(value) == 1) {
      return(if (value) "true" else "false")
    } else if (is.list(value)) {
      # Recursively handle nested lists
      return(list_to_json(value))
    } else if (length(value) == 1) {
      # Single string value
      escaped_value <- gsub('"', '\\"', as.character(value))
      return(paste0('"', escaped_value, '"'))
    } else {
      # Vector - convert to array
      json_values <- sapply(value, function(v) {
        if (is.na(v)) {
          "null"
        } else if (is.numeric(v)) {
          as.character(v)
        } else if (is.logical(v)) {
          if (v) "true" else "false"
        } else {
          escaped_v <- gsub('"', '\\"', as.character(v))
          paste0('"', escaped_v, '"')
        }
      })
      return(paste0("[", paste(json_values, collapse = ","), "]"))
    }
  }

  # Helper function to convert a list to JSON object
  list_to_json <- function(lst) {
    if (length(lst) == 0) {
      return("{}")
    }

    pairs <- sapply(names(lst), function(name) {
      value_json <- value_to_json(lst[[name]])
      paste0('"', name, '":', value_json)
    })

    return(paste0("{", paste(pairs, collapse = ","), "}"))
  }

  # Handle list input (for column definitions)
  if (is.list(data) && !is.data.frame(data)) {
    # Check if this is a list of lists (array of objects)
    if (length(data) > 0 && all(sapply(data, is.list))) {
      # Convert list of lists to JSON array
      json_objects <- sapply(data, function(obj) {
        list_to_json(obj)
      })
      return(paste0("[", paste(json_objects, collapse = ","), "]"))
    } else {
      # Single object
      return(list_to_json(data))
    }
  }

  # Handle data frame input - convert to array of objects
  rows <- list()
  for (i in seq_len(nrow(data))) {
    row <- list()
    for (j in seq_len(ncol(data))) {
      col_name <- names(data)[j]
      value <- data[i, j]
      row[[col_name]] <- value_to_json(value)
    }
    # Build row JSON
    row_json <- paste0(
      "{",
      paste(
        sapply(names(row), function(name) {
          paste0('"', name, '":', row[[name]])
        }),
        collapse = ","
      ),
      "}"
    )
    rows[[i]] <- row_json
  }
  return(paste0("[", paste(rows, collapse = ","), "]"))
}
