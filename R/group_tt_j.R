#' Convert delimiter-based column names to named list
#'
#' @keywords internal
#' @noRd
j_delim_to_named_list <- function(x, j) {
  nm <- x@names

  # Preprocess column names: add space at start/end if they start/end with delimiter
  for (i in seq_along(nm)) {
    # Check if ends with delimiter
    # Do not check if starts with delimiter, because we sometimes want empty groups in the top header
    if (endsWith(nm[i], j)) {
      nm[i] <- paste0(nm[i], " ")
    }
  }

  # Find which columns contain the delimiter
  indices <- grepl(j, nm, fixed = TRUE)

  if (!any(indices)) {
    return(list(colnames = nm, groupnames = NULL))
  }

  # For columns with delimiters, parse the hierarchical structure
  grouped_cols <- nm[indices]

  # Split each column name by the delimiter to get all levels
  split_names <- strsplit(grouped_cols, j, fixed = TRUE)
  split_lengths <- lengths(split_names)

  if (length(unique(split_lengths)) > 1) {
    delim_counts <- split_lengths - 1
    details <- paste(
      sprintf("%s (%d)", grouped_cols, delim_counts),
      collapse = ", "
    )
    stop(
      sprintf(
        "Each column name must have the same number of delimiters. Got: %s.",
        details
      ),
      call. = FALSE
    )
  }
  max_levels <- max(lengths(split_names))

  # Build the nested grouping structure from the outermost level down
  groupings <- list()
  current_colnames <- nm

  for (level in 1:max_levels) {
    # Extract the prefix for this level (first 'level' parts)
    prefixes <- character(length(grouped_cols))
    for (i in seq_along(split_names)) {
      if (length(split_names[[i]]) >= level) {
        prefixes[i] <- paste(split_names[[i]][1:level], collapse = j)
      } else {
        prefixes[i] <- paste(split_names[[i]], collapse = j)
      }
    }

    # Extract just the group name at this level
    level_names <- character(length(grouped_cols))
    for (i in seq_along(split_names)) {
      if (length(split_names[[i]]) >= level) {
        level_names[i] <- split_names[[i]][level]
      } else {
        level_names[i] <- ""
      }
    }

    unique_level_names <- unique(level_names[level_names != ""])
    level_groups <- list()
    
    for (level_name in unique_level_names) {
      unique_prefixes <- unique(prefixes[level_names == level_name])
      for (prefix in unique_prefixes) {
        matching_indices <- which(prefixes == prefix)
        col_indices <- (which(indices))[matching_indices]

        # Only create groupings with more than one column or if it's not the final level
        if (length(col_indices) > 1 || level < max_levels) {
          # Use the level name as the group name
          group_name <- level_name
          
          # Handle empty group names by replacing with a space (as per tinytable convention)
          if (is.na(group_name) || group_name == "") {
            group_name <- " "
          }

          # If this group name already exists, append to it
          if (group_name %in% names(level_groups)) {
            level_groups[[group_name]] <- c(level_groups[[group_name]], col_indices)
          } else {
            level_groups[[group_name]] <- col_indices
          }
        }
      }
    }

    # Only add level if it has meaningful groups
    if (length(level_groups) > 0) {
      groupings <- c(groupings, list(level_groups))
    }
  }

  # Set final column names (the last part after all delimiters)
  for (i in which(indices)) {
    parts <- split_names[[which(which(indices) == i)]]
    final_name <- parts[length(parts)]

    # Handle empty final names
    if (is.na(final_name) || final_name == "") {
      final_name <- " "
    }

    current_colnames[i] <- final_name
  }

  out <- list(colnames = current_colnames, groupnames = groupings)
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
