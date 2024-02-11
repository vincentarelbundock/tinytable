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
        out <- htmlEscape(out)

    } else if (isTRUE(output == "typst")) {
        out <- gsub("<", "\\<", out, fixed = TRUE)
        out <- gsub(">", "\\>", out, fixed = TRUE)
    }

    return(out)
}




# function copied from `htmltools` under GPL3 license on 2024-02-07
# https://cran.r-project.org/web/packages/htmltools/index.html
htmlEscape <- local({

  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse='|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(names(.htmlSpecialsAttrib), collapse='|')

  function(text, attribute=FALSE) {
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib
    else
      .htmlSpecialsPattern

    text <- enc2utf8(as.character(text))
    # Short circuit in the common case that there's nothing to escape
    if (!any(grepl(pattern, text, useBytes = TRUE)))
      return(text)

    specials <- if(attribute)
      .htmlSpecialsAttrib
    else
      .htmlSpecials

    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed = TRUE, useBytes = TRUE)
    }
    Encoding(text) <- "UTF-8"

    return(text)
  }
})
