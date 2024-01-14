align_str_left <- function(x, pad_n = NULL) {
    if (is.null(pad_n)) pad_n <- max(nchar(x))
    right <- strrep(" ", pad_n - nchar(x))
    paste0(x, right)
}

align_str_right <- function(x, pad_n = NULL) {
    if (is.null(pad_n)) pad_n <- max(nchar(x))
    left <- strrep(" ", pad_n - nchar(x))
    paste0(left, x)
}

align_str_center <- function(x, pad_n = NULL) {
    if (is.null(pad_n)) pad_n <- max(nchar(x))
    left <- strrep(" ", ceiling((pad_n - nchar(x)) / 2))
    right <- strrep(" ", floor((pad_n - nchar(x)) / 2))
    paste0(left, x, right)
}


tt_markdown <- function(tab, caption, ...) {

  # fake spans
  if (!is.null(colnames(tab))) {
    colnames(tab) <- gsub("\\|{4}", " / ", colnames(tab))
  }

  # align content
  # this is an infrastructure already. Can be used when 
  align <- rep("l", ncol(tab))
  for (i in seq_along(tab)) {

    # otherwise we can't take nchar()
    tab[[i]] <- as.character(tab[[i]])

    pad_n <- 0
    if (!is.null(colnames)) pad_n <- max(pad_n, nchar(colnames(tab)[i]))
    pad_n <- max(pad_n, max(nchar(as.character(tab[[i]]))))
    if (align[[i]] == "l") {
      tab[[i]] <- align_str_left(tab[[i]], pad_n = pad_n)
    } else if (align[[i]] == "r") {
      tab[[i]] <- align_str_right(tab[[i]], pad_n = pad_n)
    } else if (align[[i]] == "c") {
      tab[[i]] <- align_str_center(tab[[i]], pad_n = pad_n)
    }
  }

  # bind centered column names
  if (!is.null(colnames(tab))) {
    header <- as.data.frame(as.list(colnames(tab)))
    colnames(header) <- colnames(tab)
    for (i in seq_along(tab)) {
      header[[i]] <- align_str_center(header[[i]], nchar(tab[[i]][1]))
    }
    tab <- rbind(header, tab)
  }

  # pipes
  tab[[1]] <- paste("|", tab[[1]])
  for (i in seq_along(tab)) {
    tab[[i]] <- paste(tab[[i]], "| ")
  }
  tab <- do.call(paste0, tab)

  # ruler
  ruler <- gsub("[^\\|]", " ", tab[1])
  ruler <- gsub(" ", "-", ruler)
  ruler <- gsub("-\\|", ":|", ruler) # all except first
  ruler <- sub(":\\|", "-|", ruler) # all except first
  ruler <- sub("\\|-", "|:", ruler) # only first
  ruler <- gsub("-$", "", ruler) # only first

  hrule <- NULL
  if (!is.null(colnames(tab))) {
    hrule <- 1
  } 
  for (h in hrule) {
    tab <- append(tab, ruler, after = h)
  }

  # caption
  if (!is.null(caption)) {
    tab <- c(paste("Table:", caption), "", tab)
  }

  # # notes
  # if (!is.null(notes)) {
  #   for (n in notes) {
  #     tab <- c(tab, "", "__Note:__", paste("^^", n))
  #   }
  # }

  # output
  class(tab) <- c("tinytable_markdown", "knitr_kable")
  attr(tab, "format") <- "pipe"

  # output
  return(tab)
}
