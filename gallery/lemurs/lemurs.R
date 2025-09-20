library(tidyverse)
library(tinytable)
source("code/clean_data.R")
source("code/plot_functions.R")

# Caption ----
cap <- "<h3>The Lemurs at Duke University Center</h1>Lemurs are a unique group of primates native to Madagascar, an island off the coast of east Africa. Although they are related to monkeys and apes, lemurs make up a separate branch of the primate family tree and are classified as a superfamily, made up of five individual lemur families and more than 100 different species. Founded in 1966 on the campus of Duke University in Durham, NC, the Duke Lemur Center is a world leader in the study, care, and protection of lemurs—Earth’s most threatened group of mammals. The Duke Lemur Center houses the world’s largest and most diverse population of lemurs outside their native Madagascar."

# Footnotes ----
footnotes <- c(
  "*" = "Table adapted from an original table created by N. Rennie for the 2022 Posit Table Contest in November 2022.",
  "**" = 'Images used are licensed under Creative Commons licences. See <a style="color:#a1b70d" href="https://github.com/nrennie/2022-table-contest">github.com/nrennie/2022-table-contest</a> for details.',
  "***" = 'Data originally from <a style="color:#a1b70d" href="https://lemur.duke.edu/">lemur.duke.edu</a> and can be accessed on GitHub at <a style="color:#a1b70d" href="https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-24">github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-24</a>.')


# Table setup ----
# stubs for plot_tt()
lem_table <- data.frame(
  img_html = NA,
  species = lem_nums$species,
  bars = NA,
  lines = NA)


# Column names ----
header3 <- "How many <span style='color:#8a8d8f'>male</span> and <span style='color:#a1b70d'>female</span> lemurs are housed at Duke Lemur Center?<br><small><i>Darker shade indicates current residents.</i></small>"
header4 <- "How do <span style='color:#8a8d8f'>male</span> and <span style='color:#a1b70d'>female</span> lemurs grow as they age?<br><small><i>Average across male and female lemurs.</i></small>"
lem_table <- setNames(lem_table, c("", "", header3, header4))


# Create table ----
tab <- tt(lem_table,
  caption = cap,
  width = c(1.2, 3, 3, 3.5), # relative widths of columns
  notes = footnotes) |>
  # themes
  theme_striped() |>
  theme_html(class = "table caption-top") |> # bootstrap class
  # lemur images
  plot_tt(
    j = 1,
    images = paste0("images/", lem_nums$taxon, ".png"),
    height = 7) |>
  # bar plots
  plot_tt(
    j = 3, fun = plot_bar,
    data = as.list(lem_nums$taxon),
    df = lem_count,
    height = 5,
    height_plot = 600,
    width_plot = 3400) |>
  # line plots
  plot_tt(
    j = 4, fun = plot_line,
    data = as.list(lem_nums$taxon),
    df = lem_weights,
    height = 9,
    height_plot = 500,
    width_plot = 800) |>
  # alignment: vertical + horizontal
  style_tt(j = 1, alignv = "m") |>
  style_tt(i = 1:nrow(lem_table), j = 3:4, align = "c", alignv = "m") |>
  style_tt(i = "colnames", alignv = "t")

save_tt(tab, "lemurs.html", overwrite = TRUE)
# print(tab, "html")
