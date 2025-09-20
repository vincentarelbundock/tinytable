# Wine Table Generator using tinytable
#
# The code used to create this table was adapted to use the [`tinytable`](https://vincentarelbundock.github.io/tinytable/) package in R, based on code originally written by [Abdoul Madjid](https://www.abdoulblog.com) ([GitHub](https://github.com/AbdoulMa)) as a [submission to the RStudio Table Contest 2021](https://posit.co/blog/rstudio-table-contest-2021/), with the original code available on [GitHub](https://github.com/AbdoulMa/RStudio-Table-Contest-2021). The table uses data on some of the world's most prestigious wines from [Vivino](https://www.vivino.com/), including names, vintages, grape varieties, regions, prices, ratings, allergens, and food pairings, along with the original icons and visual elements.

library(tinytable)
library(countrycode)
source("clean_data.R")

# domain & year stacked in the same cell
df$domain_year <- sprintf("%s<br>%s", df$viticultural_domain, df$wine_year)

# ratings column: rating + stars + # reviews
rating_values <- sprintf("%.1f", df$wine_rating)
rating_styled <- style_vector(rating_values, bold = TRUE, fontsize = 1.5)
stars <- sapply(round(df$wine_rating), \(x) strrep("⭐", x))
stars_styled <- style_vector(stars, color = "#FFD700")
df$rating <- sprintf("%s<br>%s<br>%s reviews", rating_styled, stars_styled, df$wine_rating_nb)

# grape images side-by-side
# df$grape_images is a list column, where each entry is a vector of file paths to grape images
# plot_vector() returns a character vector of HTML <img> tags, one per image
grapes <- sapply(df$grape_images, \(x) plot_vector(images = x, height = 4))
df$grapes <- sapply(grapes, paste, collapse = "")

# food pairing images side-by-side
pairings <- sapply(df$pairing_images, \(x) plot_vector(images = x, height = 2.5))
df$pairings <- sapply(pairings, paste, collapse = "")

# country flags + regions stacked in single cells
flags <- countrycode::countryname(df$country, "unicode.symbol")
flags_styled <- style_vector(flags, fontsize = 2.5)
df$region <- sprintf("%s<br>%s", flags_styled, df$region)


# final table data (column order + header tweak)
cols <- c(
  "bottle_image", "wine_name", "domain_year", "region", "grapes", "pairings",
  "rating", "bottle_price_dollar", "alcohol_content")
tbl <- df[, cols]


# column names
header_icons <- c(
  "wine_glass.svg", "domain.svg", "region.svg", "grape.svg",
  "foods_pairing.svg", "star.svg", "price.svg", "alcohol_content.svg")
header_icons <- file.path("data", "columns", header_icons)
colnames(tbl)[1] <- "" # empty first header


# draw the table
tab <- tt(tbl,
  caption = "<span style='font-style: italic; font-size: 1.5em;'>Exceptional Wines</span><br>Great wines improve with age. Let's dive into some of the most extraordinary cuvées in the world. Those whose grapes possess ethereal aromas and pure minerality that give focus and energy. Those demonstrating great character, balance with good acidity and plush tannins.",
  notes = "Adapted from [a beautiful table by Abdoul Madjid.](https://github.com/AbdoulMa/RStudio-Table-Contest-2021)",
  width = c(1, 4, 5, 3, 5, 6, 3, 2, 2)) |>
  theme_empty() |>
  theme_html(class = "table table-hover table-responsive-sm caption-top") |>
  format_tt(j = 8, fn = scales::label_currency()) |>
  format_tt(j = 9, fn = scales::label_percent(scale = 1)) |>
  format_tt("notes", markdown = TRUE) |>
  style_tt(align = "c", alignv = "m") |>
  style_tt(i = 0, line = "b", line_width = 0.2) |>
  style_tt(i = 1:nrow(tbl), j = 9, color = "burgundy", bold = TRUE, fontsize = 1.5) |>
  plot_tt(i = 0, j = 2:9, images = header_icons, height = 3) |>
  plot_tt(j = 1, images = df$bottle_image, height = 4)

save_tt(tab, output = "wines.html", overwrite = TRUE)
# print(tab, "html")
