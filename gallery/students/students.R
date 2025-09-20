# The code used to create this table was adapted to use the [`tinytable`](https://vincentarelbundock.github.io/tinytable/) package in R, based on code originally published on [Illak Blog](https://illak-blog.netlify.app/posts/tabla_gt_se/tablas_gt_se).

library(tinytable)
library(paletteer)
library(scales)

data <- read.csv("data/datos_se_2023.csv")


# formatters
fmt_n <- scales::label_number(accuracy = 1, big.mark = ",", decimal_mark = ".")
fmt_pc <- scales::label_percent()


# stack number and pertanage in a single cell
n_above_pct <- function(num, den) {
  prefix <- "<span style='line-height:21px'>"
  pct <- ifelse(!is.na(num) & !is.na(den) & den > 0, num / den, NA_real_)
  ifelse(!is.na(num) & num > 0,
    paste0(prefix, fmt_n(num), "<br><i>(", fmt_pc(pct), ")</i></span>"),
    "")
}

tabla_final <- with(data, {
  data.frame(
    Modalidad,
    Nivel,
    UE_Total = UE_Total,
    UE_Estatal = n_above_pct(UE_Estatal, UE_Total),
    UE_Percent = UE_Estatal / UE_Total,
    UE_Privado = n_above_pct(UE_Privado, UE_Total),
    ALUM_Total,
    ALUM_Estatal = n_above_pct(ALUM_Estatal, ALUM_Total),
    ALUM_Percent = ALUM_Estatal / ALUM_Total,
    ALUM_Privado = n_above_pct(ALUM_Privado, ALUM_Total),
    PD_Total,
    PD_Estatal = n_above_pct(PD_Estatal, PD_Total),
    PD_Percent = PD_Estatal / PD_Total,
    PD_Privado = n_above_pct(PD_Privado, PD_Total)
  )
})


# foreground and background colors
get_color_background <- function(values, palette_name, n = 10, na_color = NA) {
  # palette from string like "cartography::purple.pal"
  pal <- as.character(paletteer::paletteer_dynamic(palette_name, n))
  col_fun <- scales::col_numeric(
    palette = pal,
    domain = range(values, na.rm = TRUE),
    na.color = na_color
  )
  bg <- col_fun(values)
  # luminance → white/black text
  hex_luma <- function(hex) {
    m <- grDevices::col2rgb(hex) / 255
    0.2126 * m[1, ] + 0.7152 * m[2, ] + 0.0722 * m[3, ]
  }
  fg <- ifelse(is.na(bg), NA, ifelse(hex_luma(bg) < 0.5, "white", "black"))
  list(bg = bg, fg = fg)
}
ue <- get_color_background(tabla_final$UE_Total, "cartography::purple.pal")
alu <- get_color_background(tabla_final$ALUM_Total, "cartography::wine.pal")
pd <- get_color_background(tabla_final$PD_Total, "cartography::blue.pal")


# column names
idx <- grepl("moda|nivel|total|percent", colnames(tabla_final), ignore.case = TRUE)
colnames(tabla_final)[!idx] <- ""

idx <- grepl("Total", colnames(tabla_final))
colnames(tabla_final)[idx] <- "Total"

privadas <- style_vector("Privadas", color = "#B0AEB0", smallcap = TRUE, output = "html")
privadas <- paste0("<br>", privadas)
colnames(tabla_final)[colnames(tabla_final) == "UE_Percent"] <- paste0(
  style_vector("Estatales", color = "#5E2A72", smallcap = TRUE),
  privadas)
colnames(tabla_final)[colnames(tabla_final) == "ALUM_Percent"] <- paste0(
  style_vector("Estatales", color = "#D5676D", smallcap = TRUE),
  privadas)
colnames(tabla_final)[colnames(tabla_final) == "PD_Percent"] <- paste0(
  style_vector("Estatales", color = "#4F91B4", smallcap = TRUE),
  privadas)


# column spanners
# plot_vector() returns a vector of <img> tags
img <- plot_vector(
  images = c(
    "data/school-building-with-flag-svgrepo-com.svg",
    "data/student-svgrepo-com.svg",
    "data/teacher-svgrepo-com.svg"
  ),
  height = 2)
img <- paste0(img, "<br>", c("Unidades Educativas", "Alumnos", "Personal Docente"))
span <- setNames(list(3:6, 7:10, 11:14), img)


# relative widths and column alignment
w <- rep(1, 14)
w[1] <- 1.6
w[c(2, 5, 9, 13)] <- 1.2


# draw table
tab <- tt(tabla_final,
  width = w,
  notes = "Adapted from [a table by Illak Zapata](https://illak-blog.netlify.app).") |>
  # bootstrap theme removes default grid
  theme_html(class = "table table-borderless") |>
  # format numbers, missing values, and markdown notes
  format_tt(j = c(3, 7, 11), fn = fmt_n) |>
  format_tt(replace = TRUE) |> # NA -> ""
  format_tt("notes", markdown = TRUE) |>
  # percentage bars
  plot_tt(
    j = 5, height = 1.2, height_plot = 600, width_plot = 2400,
    fun = "barpct", color = "#5e2a72", background = "#B0AEB0",
    data = as.list(tabla_final[[5]])) |>
  plot_tt(
    j = 9, height = 1.2, height_plot = 600, width_plot = 2400,
    fun = "barpct", color = "#d5676d", background = "#B0AEB0",
    data = as.list(tabla_final[[9]])) |>
  plot_tt(
    j = 13, height = 1.2, height_plot = 600, width_plot = 2400,
    fun = "barpct", color = "#4f91b4", background = "#B0AEB0",
    data = as.list(tabla_final[[13]])) |>
  # span: column 1
  style_tt(i = 1, j = 1, rowspan = 4) |>
  style_tt(i = 7, j = 1, rowspan = 2) |>
  # span: rows
  group_tt(j = span) |>
  # style: alignment
  style_tt(align = "llcrclcrclcrcl") |>
  style_tt(alignv = "m") |>
  style_tt(i = 0, alignv = "b") |>
  # style: font
  style_tt(i = 0, smallcap = TRUE) |>
  style_tt(i = "groupj", smallcap = TRUE) |>
  # style: horizontal rules
  strip_tt(line = TRUE) |>
  style_tt(i = 1, line = "t", line_color = "black", line_width = .15) |>
  style_tt(i = c(2:4, 8), line = "t", line_color = "white", line_width = .05) |>
  style_tt(i = c(5:7, 9:10), line = "t", line_color = "black", line_width = .05) |>
  # style: colors
  style_tt(i = 1:nrow(tabla_final), j = 3, color = ue$fg, background = ue$bg, bold = TRUE) |>
  style_tt(i = 1:nrow(tabla_final), j = 7, color = alu$fg, background = alu$bg, bold = TRUE) |>
  style_tt(i = 1:nrow(tabla_final), j = 11, color = pd$fg, background = pd$bg, bold = TRUE)

save_tt(tab, "students.html")
# print(tab, "html")
