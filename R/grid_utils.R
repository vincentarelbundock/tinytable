# ANSI-aware utility functions
ansi_strwrap <- function(text, width) {
    # For ANSI text, only wrap if visual width exceeds target width
    visual_width <- calculate_text_width(text)
    if (visual_width <= width) {
        return(text)
    } else {
        # If it's too long, fall back to regular strwrap
        # This is a simplified implementation - could be enhanced for better ANSI handling
        return(strwrap(text, width = width))
    }
}

ansi_format <- function(txt, width) {
    formatted <- character(length(txt))
    for (i in seq_along(txt)) {
        visual_width <- calculate_text_width(txt[i])
        if (visual_width < width) {
            padding_needed <- width - visual_width
            formatted[i] <- paste0(txt[i], strrep(" ", padding_needed))
        } else {
            formatted[i] <- txt[i]
        }
    }
    return(formatted)
}
