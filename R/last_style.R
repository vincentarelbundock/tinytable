last_valid <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    return(utils::tail(x, 1))
}

last_style_vec <- function(x) {
    if (is.factor(x)) {
        x <- as.character(x)
    }
    if (is.logical(x) && !all(is.na(x))) {
        x <- any(sapply(x, isTRUE)) 
    } else {
        x <- last_valid(x)
    }
    return(x)
}


last_style <- function(sty) {
    sty <- split(sty, list(sty$i, sty$j))
    sty <- lapply(sty, function(k) lapply(k, last_style_vec))
    sty <- do.call(rbind, lapply(sty, data.frame))
    return(sty)
}
