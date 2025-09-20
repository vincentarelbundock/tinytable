# Data cleaning and image path matching for wine table
# This script cleans the wine data and creates list columns with image paths

# Helper functions
slugify <- function(x) {
  x <- gsub("[^[:alnum:] ]", "", x)
  x <- gsub(" ", "_", x)
  x <- tolower(x)
  return(x)
}

# Clean input data function
clean_input_data <- function(wines_df) {
  df <- wines_df

  # year token and cleaned wine_name
  m <- regexpr("(\\d{4}|N\\.V)", df$wine_name, perl = TRUE)
  df$wine_year <- ifelse(m > 0, regmatches(df$wine_name, m), NA_character_)
  df$wine_name <- sub("\\s(\\d{4}|N\\.V)$", "", df$wine_name, perl = TRUE)

  # ratings
  df$wine_rating <- as.numeric(sub(",", ".", df$wine_rating))
  df$wine_rating_nb <- trimws(sub("notes", "", df$wine_rating_nb, perl = TRUE))

  # bottle_price: keep digits/comma/dot/sign, normalize comma, numeric
  bp <- gsub("\\s+", "", df$bottle_price, perl = TRUE)
  bp <- gsub("[^0-9,.-]", "", bp, perl = TRUE)
  bp <- gsub(",", ".", bp, perl = TRUE)
  df$bottle_price <- suppressWarnings(as.numeric(bp))
  df$bottle_price_dollar <- ceiling((df$bottle_price * 1.16068) / 5) * 5

  # alcohol_content: strip %, normalize comma, numeric
  ac <- gsub("%", "", df$alcohol_content, fixed = TRUE)
  ac <- gsub("[^0-9,.-]", "", ac, perl = TRUE)
  ac <- gsub(",", ".", ac, perl = TRUE)
  df$alcohol_content <- suppressWarnings(as.numeric(ac))

  df <- stats::na.omit(df)
  rownames(df) <- NULL
  df
}

# Food pairing icon mapping
food_to_icon <- c(
  "Agneau" = "agneau.svg",
  "Apéritif" = "apéritif.svg",
  "Apéritif et snacks" = "apéritif_et_snacks.svg",
  "Bœuf" = "bœuf.svg",
  "Champignons" = "champignons.svg",
  "Crustacés" = "crustacés.svg",
  "Desserts fruités" = "desserts_fruités.svg",
  "Desserts sucrés" = "desserts_sucrés.svg",
  "Fromage affiné et à pâte dure" = "fromage_affiné_et_à_pâte_dure.svg",
  "Fromage bleu" = "fromage_bleu.svg",
  "Fromage de chèvre" = "fromage_de_chèvre.svg",
  "Fromage doux et à pâte molle" = "fromage_doux_et_à_pâte_molle.svg",
  "Gibier" = "gibier.svg",
  "Mets épicés" = "mets_épicés.svg",
  "N'importe quelle camelote alimentaire fera l'affaire" = "n'importe_quelle_camelote_alimentaire_fera_l'affaire.svg",
  "Poisson gras" = "poisson_gras.svg",
  "Poisson maigre" = "poisson_maigre.svg",
  "Porc" = "porc.svg",
  "Pâtes" = "pâtes.svg",
  "Veau" = "veau.svg",
  "Viande assaisonnée" = "viande_assaisonnée.svg",
  "Volaille" = "volaille.svg",
  "Végétarien" = "végétarien.svg"
)

# Function to get grape image paths
get_grape_images <- function(grape_variety) {
  # map variety name -> image path (fallback to others.png)
  get_grape_image <- function(grape_variety) {
    default <- file.path("data", "grapes", "others.png")
    candidate <- file.path("data", "grapes", paste0(grape_variety, ".png"))
    if (file.exists(candidate)) candidate else default
  }

  # remove percentages like "85 % " or "85% ", trim, split on commas, take first 4
  clean <- trimws(gsub("[0-9]|%", "", grape_variety))
  parts <- strsplit(clean, ",", perl = TRUE)[[1]]
  parts <- gsub("^_+|_+$", "", slugify(parts), perl = TRUE)
  parts <- head(unique(parts), 4)

  # get image paths
  sapply(parts, get_grape_image, USE.NAMES = FALSE)
}

# Function to get pairing image paths
get_pairing_images <- function(pairing) {
  pairing_clean <- gsub("\\(.*?\\)", "", pairing, perl = TRUE)
  pairing_items <- trimws(strsplit(pairing_clean, ",", fixed = FALSE)[[1]])

  icons <- vapply(pairing_items, function(item) {
    icon_file <- food_to_icon[[item]]
    if (!is.na(icon_file)) {
      file.path("data", "pairings", icon_file)
    } else {
      file.path("data", "pairings", paste0(item, ".svg"))
    }
  }, character(1L))

  # drop any NA results
  icons[!is.na(icons)]
}

# Load and clean the wine data
wines <- read.csv("data/wines_top100.csv")
wines <- clean_input_data(wines)

# subset and sort
df <- wines[c(1:4, 7:9), , drop = FALSE]
df <- df[order(-df$wine_rating, -df$bottle_price_dollar), ]

# Set bottle image paths
df$bottle_image <- file.path("data/bottles", basename(df$bottle_image))

# Create list columns with image paths
df$grape_images <- lapply(df$grape_variety, get_grape_images)
df$pairing_images <- lapply(df$food_pairing, get_pairing_images)

# Process regions for flag styling
regions <- strsplit(df$region, "/", fixed = TRUE)
df$country <- sapply(regions, \(x) trimws(x[[1]]))
df$region <- sapply(regions, \(x) trimws(x[[2]]))

