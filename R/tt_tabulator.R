setMethod(
  f = "tt_eval",
  signature = "tinytable_tabulator",
  definition = function(x, ...) {
    template <- readLines(
      system.file("templates/tabulator.html", package = "tinytable")
    )

    # (pseudo-)unique table IDs
    id <- get_id("")
    x@id <- id

    # Replace table ID in template
    template <- gsub(
      "$tinytable_TABLE_ID",
      paste0("tinytable_", id),
      template,
      fixed = TRUE
    )

    # Convert data to JSON for Tabulator
    # Clean column names (replace dots with underscores)
    data_clean <- x@data_body
    names(data_clean) <- gsub("\\.", "_", names(data_clean))
    
    # Clean problematic quotes in character columns
    data_clean <- as.data.frame(lapply(data_clean, function(col) {
      if (is.character(col)) {
        gsub('"', "'", col) # Replace double quotes with single quotes
      } else {
        col
      }
    }))

    # Convert to JSON
    js_data <- jsonlite::toJSON(
      data_clean,
      dataframe = "rows",
      auto_unbox = TRUE,
      pretty = FALSE,
      na = "null",
      force = TRUE
    )

    # Build column definitions
    columns <- lapply(names(data_clean), function(nm) {
      original_name <- colnames(x@data_body)[which(gsub("\\.", "_", colnames(x@data_body)) == nm)]
      # Use original data for type detection since @data_body is processed to character
      col_data <- x@data[[original_name]]
      col_type <- class(col_data)[1]
      
      # Basic column definition
      col_def <- list(
        title = original_name,
        field = nm
      )
      
      # Add basic formatter based on type
      if (col_type %in% c("integer", "numeric", "double")) {
        col_def$formatter <- "number"
        if (col_type == "integer") {
          col_def$formatterParams <- list(decimal = ".", thousand = ",", precision = 0)
        } else {
          col_def$formatterParams <- list(decimal = ".", thousand = ",", precision = 2)
        }
      } else if (col_type == "logical") {
        col_def$formatter <- "tickCross"
      } else if (col_type == "Date") {
        col_def$formatter <- "date"
        col_def$formatterParams <- list(outputFormat = "YYYY-MM-DD")
      } else if (col_type %in% c("POSIXct", "POSIXlt")) {
        col_def$formatter <- "date"
        col_def$formatterParams <- list(outputFormat = "YYYY-MM-DD HH:mm:ss")
      } else {
        col_def$formatter <- "plaintext"
      }
      
      return(col_def)
    })

    js_columns <- jsonlite::toJSON(columns, auto_unbox = TRUE, pretty = FALSE)

    # Replace data and columns in template
    template <- gsub(
      "$tinytable_TABULATOR_DATA",
      js_data,
      template,
      fixed = TRUE
    )
    
    template <- gsub(
      "$tinytable_TABULATOR_COLUMNS",
      js_columns,
      template,
      fixed = TRUE
    )

    # Add default CDN (bootstrap5)
    template <- gsub(
      "$tinytable_TABULATOR_CDN",
      '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/tabulator_bootstrap5.min.css" rel="stylesheet">',
      template,
      fixed = TRUE
    )

    # Leave options placeholder for potential theme replacement
    # (it will be cleaned up in finalize if not replaced)

    out <- paste(template, collapse = "\n")

    # Store table string for potential styling later
    x@table_string <- out

    return(x)
  }
)

# Helper function to set up tabulator-specific S4 class
tabulator_setting <- function(x, new, component = "option") {
  att <- attributes(x)
  out <- strsplit(x, "\n")[[1]]
  
  if (component == "option") {
    # Find the options line and insert before it
    idx <- grep("$tinytable_TABULATOR_OPTIONS", out, fixed = TRUE)
    if (length(idx) > 0) {
      out <- c(
        out[1:(idx - 1)],
        new,
        out[idx:length(out)]
      )
    }
  }
  
  out <- paste(out, collapse = "\n")
  attributes(out) <- att
  class(out) <- class(x)
  return(out)
}