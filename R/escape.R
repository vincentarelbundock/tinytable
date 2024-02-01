escape_text <- function(x, output = "latex") {
    if (length(x) < 1 || all(is.na(x))) {
        return(x)
    }

    out <- x

    if (isTRUE(output == "latex")) {
        # LaTeX escaping code adapted from the `gt` package, published under MIT
        # https://github.com/rstudio/gt/
        # YEAR: 2018-2024
        # COPYRIGHT HOLDER: gt authors
        # If all text elements are `NA_character_` then return `text` unchanged
        latex_special_chars <- c(
            "\\" = "\\textbackslash{}",
            "~" = "\\textasciitilde{}",
            "^" = "\\textasciicircum{}",
            "&" = "\\&",
            "%" = "\\%",
            "$" = "\\$",
            "#" = "\\#",
            "_" = "\\_",
            "{" = "\\{",
            "}" = "\\}"
        )
        na_out <- is.na(out)
        m <- gregexpr("[\\\\&%$#_{}~^]", out[!na_out], perl = TRUE)
        special_chars <- regmatches(out[!na_out], m)
        escaped_chars <- lapply(special_chars, function(x) {
            latex_special_chars[x]
        })
        regmatches(out[!na_out], m) <- escaped_chars
    } else if (isTRUE(output == "html")) {
        assert_dependency("htmltools")
        out <- htmltools::htmlEscape(out)
    }

    return(out)
}