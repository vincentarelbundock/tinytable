#' tinytable S4 method
#' 
#' @keywords internal
style_eval_grid <- function(x,
                            i = NULL,
                            j = NULL,
                            bold = FALSE,
                            italic = FALSE,
                            monospace = FALSE,
                            underline = FALSE,
                            strikeout = FALSE,
                            rowspan = NULL,
                            colspan = NULL,
                            ...) {


  out <- x@table_dataframe
  sty <- x@style

  all_i <- seq_len(nrow(x))
  idx_g <- x@group_i_idx + cumsum(rep(1, length(x@group_i_idx))) - 1
  idx_d <- setdiff(all_i, idx_g)

  # expand i to full rows
  if (any(is.na(sty$i))) {
    alli <- data.frame(i = seq_len(nrow(x)))
    alli <- merge(alli, sty[is.na(sty$i), colnames(sty) != "i"], all = TRUE)
    sty <- rbind(sty, alli)
    sty <- sty[!is.na(sty$i),]
    sty <- sty[order(sty$i, sty$j),]
  }
  
  last <- function(k) {
    if (all(is.na(k))) return(NA)
    if (is.logical(k)) return(as.logical(max(k, na.rm = TRUE)))
    return(utils::tail(stats::na.omit(k), 1))
  }
  sty <- do.call(rbind, by(sty, list(sty$i, sty$j), function(k) {
    data.frame(lapply(k, last))
  }))

  # TODO: style groups
  sty <- sty[which(!sty$i %in% idx_g),]

  if (nrow(sty) == 0) return(x)

  # user-supplied indices are post-groups
  # adjust indices to match original data rows since we only operate on those 
  for (g in rev(idx_g)) {
    sty[sty$i > g, "i"] <- sty[sty$i > g, "i"] - 1
  }

  for (col in seq_along(out)) {
    out[[col]] <- as.character(out[[col]])
  }

  for (idx in seq_len(nrow(sty))) {
    row <- sty[idx, "i"]
    col <- sty[idx, "j"]
    bold <- sty[which(sty$i == row & sty$j == col), "bold"]
    italic <- sty[which(sty$i == row & sty$j == col), "italic"]
    strikeout <- sty[which(sty$i == row & sty$j == col), "strikeout"]
    rowspan <- sty[which(sty$i == row & sty$j == col), "rowspan"]
    colspan <- sty[which(sty$i == row & sty$j == col), "colspan"]
    if (isTRUE(bold)) {
      out[row, col] <- sprintf("**%s**", out[row, col])
    }
    if (isTRUE(italic)) {
      out[row, col] <- sprintf("*%s*", out[row, col])
    }
    if (isTRUE(strikeout)) {
      out[row, col] <- sprintf("~~%s~~", out[row, col])
    }
    if (!is.null(rowspan) || !is.null(colspan)) {
      idx_row <- if (isTRUE(rowspan > 1)) row + seq_len(rowspan) - 1 else row
      idx_col <- if (isTRUE(colspan > 1)) col + seq_len(colspan) - 1 else col
      backup <- out[row, col]
      for (w in idx_row) {
        for (z in idx_col) {
          if (z <= x@ncol) {
            out[w, z] <- ""
          }
        }
      }
      out[row, col] <- backup
    }
  }

  x@table_dataframe <- out
  return(x)
}




#' tinytable S4 method
#' 
#' @keywords internal
setMethod(
          f = "style_eval",
          signature = "tinytable_grid",
          definition = style_eval_grid
)
