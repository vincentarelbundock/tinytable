# (pseudo-)unique IDs
get_id <- function(stem = "id") {
  id <- sample(c(0:9, letters), 20, replace = TRUE)
  paste0(stem, paste(id, collapse = ""))
}


meta <- function(x, get, set = NULL) {
    # Get the 'tinytable_meta' attribute; if NULL, initialize as an empty list
    meta_attr <- attr(x, "tinytable_meta")
    if (is.null(meta_attr)) {
        meta_attr <- list()
    }
    
    if (!is.null(set)) {
        # If 'set' is not NULL, replace or create the 'get' element with 'set'
        meta_attr[[get]] <- set
        attr(x, "tinytable_meta") <- meta_attr
        return(x)
    } else {
        # If 'set' is NULL, return the 'get' element (or NULL if it does not exist)
        return(meta_attr[[get]])
    }
}

