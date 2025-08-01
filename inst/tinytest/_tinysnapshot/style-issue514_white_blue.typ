#show figure: set block(breakable: true)
#figure( // start preamble figure
  
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_0": 0, "1_0": 0, "0_1": 0, "1_1": 0, "0_2": 0, "1_2": 0, "0_3": 0, "1_3": 0
  )

  #let style-array = ( 
    // tinytable cell style after
    (color: white, background: rgb("#0000FF"),),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 0, start: 0, end: 4, stroke: 0.5em + white), table.hline(y: 1, start: 0, end: 4, stroke: 0.5em + white),
 table.hline(y: 1, start: 0, end: 4, stroke: 0.5em + white), table.hline(y: 2, start: 0, end: 4, stroke: 0.5em + white),
 table.vline(x: 0, start: 0, end: 2, stroke: 0.5em + white), table.vline(x: 1, start: 0, end: 2, stroke: 0.5em + white),
 table.vline(x: 1, start: 0, end: 2, stroke: 0.5em + white), table.vline(x: 2, start: 0, end: 2, stroke: 0.5em + white),
 table.vline(x: 2, start: 0, end: 2, stroke: 0.5em + white), table.vline(x: 3, start: 0, end: 2, stroke: 0.5em + white),
 table.vline(x: 3, start: 0, end: 2, stroke: 0.5em + white), table.vline(x: 4, start: 0, end: 2, stroke: 0.5em + white),
    // tinytable lines before

    // tinytable cell content after
[1], [3], [5], [7],
[2], [4], [6], [8],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
