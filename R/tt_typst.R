setMethod(
    f = "tt_eval",
    signature = "tinytable_typst",
    definition = function(x, ...) {
        out <- readLines(system.file("templates/typst.typ", package = "tinytable"))
        out <- paste(out, collapse = "\n")

        # body
        body <- apply(x@table_dataframe, 2, function(k) paste0("[", k, "]"))
        if (nrow(x@table_dataframe) && is.null(dim(body))) {
            body <- matrix(body)
        }
        header <- !is.null(colnames(x)) && length(colnames(x)) > 0
        if (header) {
            header <- paste(paste0("[", colnames(x), "]"), collapse = ", ")
            header <- paste0(header, ",")
            out <- lines_insert(out, header, "repeat: true", "after")
        }
        body <- apply(body, 1, paste, collapse = ", ", simplify = FALSE)
        body <- paste(body, collapse = ",\n")
        body <- paste0(body, ",\n")
        out <- typst_insert(out, body, type = "body")

        if (length(x@width) == 0) {
            width <- rep("auto", ncol(x))
        } else if (length(x@width) == 1) {
            width <- rep(sprintf("%.2f%%", x@width / ncol(x) * 100), ncol(x))
        } else {
            width <- sprintf("%.2f%%", x@width * 100)
        }
        width <- sprintf("    columns: (%s),", paste(width, collapse = ", "))
        out <- lines_insert(out, width, "tinytable table start", "after")

        # notes
        if (length(x@notes) > 0) {
            ft <- "
    table.footer(
      repeat: false,
      // tinytable notes after
    ),
    "
            out <- lines_insert(out, ft, "tinytable footer after", "after")
            notes <- rev(x@notes)
            # otherwise an empty caption is created automatically
            if (is.null(names(notes))) {
                lab <- rep("", length(notes))
            } else {
                lab <- names(notes)
            }
            notes <- sapply(notes, function(n) if (is.list(n)) n$text else n)
            for (k in seq_along(notes)) {
                if (lab[k] == "") {
                    tmp <- sprintf("    table.cell(align: left, colspan: %s, %s),", ncol(x), notes[k])
                } else {
                    tmp <- sprintf("    table.cell(align: left, colspan: %s, [#super[%s] %s]),", ncol(x), lab[k], notes[k])
                }
                out <- lines_insert(out, tmp, "tinytable notes after", "after")
            }
        }

        # default alignment
        align_default <- sprintf(
            "  #let align-default-array = ( %s, ) // tinytable align-default-array here",
            paste(rep("left", ncol(x)), collapse = ", "))
        out <- lines_insert(
            out,
            align_default,
            "// tinytable align-default-array before",
            "after")

        x@table_string <- out

        return(x)
    })


typst_insert <- function(x, content = NULL, type = "body") {
    if (is.null(content)) {
        return(x)
    }

    out <- strsplit(x, "\n")[[1]]
    comment <- switch(type,
        "lines" = "tinytable lines before",
        "style" = "tinytable cell style before",
        "body" = "tinytable cell content after"
    )
    idx <- grep(comment, out)

    if (type == "body") {
        out <- c(out[1:idx], content, out[(idx + 1):length(out)])
    } else {
        out <- c(out[1:(idx - 1)], content, out[idx:length(out)])
    }

    out <- paste(out, collapse = "\n")
    return(out)
}
