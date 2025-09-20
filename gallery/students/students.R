# The code used to create this table was adapted to use the [`tinytable`](https://vincentarelbundock.github.io/tinytable/) package in R, based on code originally published on [Illak Blog](https://illak-blog.netlify.app/posts/tabla_gt_se/tablas_gt_se).

library(tinytable)
library(paletteer)
library(scales)

data <- read.csv("data/datos_se_2023.csv")


# formatters
fmt_n <- scales::label_number(accuracy = 1, big.mark = ",", decimal_mark = ".")
fmt_pc <- scales::label_percent()
smallcaps <- function(text, color = "black") {
  sprintf(
    "<span style='font-weight:bold;font-variant:small-caps;color:%s;'>%s</span>",
    color,
    text
  )
}



# single helper to calculate percentage and return formatted HTML cell
cellify <- function(num, den) {
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
    UE_Estatal = cellify(UE_Estatal, UE_Total),
    UE_Percent = UE_Estatal / UE_Total,
    UE_Privado = cellify(UE_Privado, UE_Total),
    ALUM_Total,
    ALUM_Estatal = cellify(ALUM_Estatal, ALUM_Total),
    ALUM_Percent = ALUM_Estatal / ALUM_Total,
    ALUM_Privado = cellify(ALUM_Privado, ALUM_Total),
    PD_Total,
    PD_Estatal = cellify(PD_Estatal, PD_Total),
    PD_Percent = PD_Estatal / PD_Total,
    PD_Privado = cellify(PD_Privado, PD_Total)
  )
})


# compute foreground and background colors on a color scale with range based on total values
hex_luma <- function(hex) {
  # hex -> RGB [0,1] -> relative luminance
  m <- col2rgb(hex) / 255
  0.2126 * m[1, ] + 0.7152 * m[2, ] + 0.0722 * m[3, ]
}
pal_ue <- as.character(paletteer::paletteer_dynamic("cartography::purple.pal", 10))
pal_alu <- as.character(paletteer::paletteer_dynamic("cartography::wine.pal", 10))
pal_pd <- as.character(paletteer::paletteer_dynamic("cartography::blue.pal", 10))
rng <- range(tabla_final$UE_Total, tabla_final$ALUM_Total, tabla_final$PD_Total)
col_ue <- scales::col_numeric(palette = pal_ue, domain = range(tabla_final$UE_Total), na.color = NA)
col_alu <- scales::col_numeric(palette = pal_alu, domain = range(tabla_final$ALUM_Total), na.color = NA)
col_pd <- scales::col_numeric(palette = pal_pd, domain = range(tabla_final$PD_Total), na.color = NA)
bg_ue <- col_ue(tabla_final$UE_Total)
bg_alu <- col_alu(tabla_final$ALUM_Total)
bg_pd <- col_pd(tabla_final$PD_Total)
fg_ue <- ifelse(is.na(bg_ue), NA, ifelse(hex_luma(bg_ue) < 0.5, "white", "black"))
fg_alu <- ifelse(is.na(bg_alu), NA, ifelse(hex_luma(bg_alu) < 0.5, "white", "black"))
fg_pd <- ifelse(is.na(bg_pd), NA, ifelse(hex_luma(bg_pd) < 0.5, "white", "black"))


# column names

idx <- grepl("moda|nivel|total|percent", colnames(tabla_final), ignore.case = TRUE)
colnames(tabla_final)[!idx] <- ""

colnames(tabla_final)[colnames(tabla_final) == "UE_Percent"] <- paste(
  smallcaps("Estatales", "#5e2a72"),
  smallcaps("Privadas", "#B0AEB0")
)
colnames(tabla_final)[colnames(tabla_final) == "ALUM_Percent"] <- paste(
  smallcaps("Estatales", "#D5676D"),
  smallcaps("Privadas", "#B0AEB0")
)
colnames(tabla_final)[colnames(tabla_final) == "PD_Percent"] <- paste(
  smallcaps("Estatales", "#4F91B4"),
  smallcaps("Privadas", "#B0AEB0")
)

colnames(tabla_final)[grepl("Total", colnames(tabla_final))] <- "Total"


# column spanners
img <- sprintf('<img src="%s" style="height:2em; vertical-align:middle;">', c(
  "data/school-building-with-flag-svgrepo-com.svg",
  "data/student-svgrepo-com.svg",
  "data/teacher-svgrepo-com.svg"
))
img <- paste0(img, "<br>", smallcaps(c("Unidades Educativas", "Alumnos", "Personal Docente")))
span <- setNames(list(3:6, 7:10, 11:14), img)


# relative widths and column alignment
w <- rep(1, 14)
w[1] <- 1.6
w[2] <- 1.2
w[c(5, 9, 13)] <- 1.2
a <- c("llcrclcrclcrcl")

src <- "Adapted from [a beautiful table by Illak Zapata](https://illak-blog.netlify.app)."


# draw table
tab <- tt(tabla_final, width = w, notes = src) |>
  theme_html(class = "table table-borderless") |>
  style_tt(align = a) |>
  style_tt(i = 1:10, j = 3:ncol(tabla_final), alignv = "m") |>
  style_tt(i = 0, alignv = "b") |>
  format_tt(i = 0, fn = smallcaps) |>
  format_tt(i = "groupj", fn = smallcaps) |>
  format_tt(j = c(3, 7, 11), fn = fmt_n) |>
  format_tt(replace = TRUE) |> # NA -> ""
  format_tt("notes", markdown = TRUE) |>
  # group row h-rules
  strip_tt(line = TRUE) |>
  style_tt(i = 1, line = "t", line_color = "black", line_width = .15) |>
  style_tt(i = c(2:4, 8), line = "t", line_color = "white", line_width = .05) |>
  style_tt(i = 5:7, line = "t", line_color = "black", line_width = .05) |>
  style_tt(i = 9:10, line = "tb", line_color = "black", line_width = .05) |>
  # background colors for total columns
  style_tt(i = 1:nrow(tabla_final), j = 3, color = fg_ue, background = bg_ue, bold = TRUE) |>
  style_tt(i = 1:nrow(tabla_final), j = 7, color = fg_alu, background = bg_alu, bold = TRUE) |>
  style_tt(i = 1:nrow(tabla_final), j = 11, color = fg_pd, background = bg_pd, bold = TRUE) |>
  # percentage bars
  plot_tt(
    j = 5, height = .8, asp = .2,
    fun = "barpct", color = "#5e2a72", background = "#B0AEB0",
    data = as.list(tabla_final[[5]])) |>
  plot_tt(
    j = 9, height = .8, asp = .2,
    fun = "barpct", color = "#d5676d", background = "#B0AEB0",
    data = as.list(tabla_final[[9]])) |>
  plot_tt(
    j = 13, height = .8, asp = .2,
    fun = "barpct", color = "#4f91b4", background = "#B0AEB0",
    data = as.list(tabla_final[[13]])) |>
  # spanning rows for "Modalidad"
  style_tt(i = 1, j = 1, rowspan = 4) |>
  style_tt(i = 7, j = 1, rowspan = 2) |>
  # # column spanners
  group_tt(j = span)

save_tt(tab, "tabla.html", overwrite = TRUE)
