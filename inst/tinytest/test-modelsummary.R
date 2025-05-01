source("helpers.R")
requiet("modelsummary")

# modelsummary Issue #711: stars and d-columns
training <- 'https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Treatment.csv'
training <- read.csv(training, na.strings = "")
training <- transform(
  training,
  `Earnings Before` = re75 / 1000,
  `Earnings After` = re78 / 1000,
  Treatment = ifelse(treat == TRUE, 'Treatment', 'Control'),
  Married = ifelse(married == TRUE, 'Yes', 'No')
)
cols <- c(
  "Earnings Before",
  "Earnings After",
  "Treatment",
  "ethn",
  "age",
  "educ",
  "Married"
)
names <- c(
  "Earnings Before",
  "Earnings After",
  "Treatment",
  "Ethnicity",
  "Age",
  "Education",
  "Married"
)
training <- training[, cols]
training <- setNames(training, names)
tab <- datasummary_balance(
  ~Treatment,
  training,
  fmt = 3,
  stars = TRUE,
  output = "data.frame"
)
p <- tt(tab) |> style_tt(align = "lllllldl") |> save_tt("latex")
expect_inherits(p, "character")
