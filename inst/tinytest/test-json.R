# Unit tests for the internal JSON serializer (R/json.R)
df_to_json <- tinytable:::df_to_json
json_raw <- tinytable:::json_raw
value_to_json <- tinytable:::value_to_json

# quotes, backslashes, and newlines are escaped
out <- df_to_json(data.frame(
  x = 'a"b',
  y = "back\\slash",
  z = "new\nline",
  stringsAsFactors = FALSE
))
expect_equal(out, '[{"x":"a\\"b","y":"back\\\\slash","z":"new\\nline"}]')

# leading-zero strings stay quoted strings (no numeric coercion)
expect_equal(
  df_to_json(data.frame(id = "007", stringsAsFactors = FALSE)),
  '[{"id":"007"}]'
)

# non-finite numerics -> null
expect_equal(
  df_to_json(data.frame(v = c(Inf, -Inf, NaN, NA, 1.5))),
  '[{"v":null},{"v":null},{"v":null},{"v":null},{"v":1.5}]'
)

# missing character and logical values -> null
expect_equal(
  df_to_json(data.frame(s = NA_character_, b = NA, stringsAsFactors = FALSE)),
  '[{"s":null,"b":null}]'
)

# json_raw() passes through verbatim: unquoted, unescaped
expect_equal(
  value_to_json(json_raw("function(cell){return 1;}")),
  "function(cell){return 1;}"
)
raw_df <- data.frame(s = 1)
raw_df$s <- list(json_raw("[1,2,3]"))
expect_equal(df_to_json(raw_df), '[{"s":[1,2,3]}]')
