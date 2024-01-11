

group_bootstrap <- function(x, i = TRUE, italic = TRUE, rule = TRUE, indent = 1) {
  out <- bootstrap_setting(x, "let row = table.insertRow(1);", component = "newrows")
  out <- bootstrap_setting(out, "let c1 = row.insertCell(0);", component = "newrows")
  out <- bootstrap_setting(out, "c1.innerText = 'Blah'", component = "newrows")
  out <- bootstrap_setting(out, "table.rows[0].cells[0].setAttribute('colspan', 8);")
  return(out)
}
