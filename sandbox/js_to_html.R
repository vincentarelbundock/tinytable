library(chromote)
library(tinytable)

tt(mtcars[1:3, 1:4]) |>
    style_tt(j = 1, color = "white", background = "teal") |>
    save_tt("example.html", overwrite = TRUE)

html_js_to_plain <- function(filename) {
    filename <- path.expand(filename)
    if (!file.exists(filename)) {
        msg <- sprintf("`%s` does not exist.", filename)
        stop(msg, call. = FALSE)
    }
    filename <- paste0("file:", filename)
    b <- ChromoteSession$new()
    b$Page$navigate(filename)
    b$Page$loadEventFired()
    js_code <- "var clone = document.body.cloneNode(true);
                clone.querySelectorAll('script').forEach(script => script.remove());
                clone.outerHTML;"
    body <- b$Runtime$evaluate(js_code)$result$value
    head <- b$Runtime$evaluate("document.querySelector('head').outerHTML")$result$value
    out <- paste('```{=html}', '<html lang="en">', head, body, '</html>', '```', sep = '\n')
    b$close()
    out <- knitr::asis_output(out)
    return(out)
}

filename <- "~/Downloads/example.html"
html_js_to_plain(filename)
