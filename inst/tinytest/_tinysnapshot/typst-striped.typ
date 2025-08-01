#show figure: set block(breakable: true)
#figure( // start preamble figure
  
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "1_1": 0, "3_1": 0, "1_2": 0, "3_2": 0, "1_3": 0, "3_3": 0, "1_4": 0, "3_4": 0
  )

  #let style-array = ( 
    // tinytable cell style after
    (background: rgb("#ededed"),),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
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
    columns: (auto, auto, auto, auto, auto),
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
 table.hline(y: 5, start: 0, end: 5, stroke: 0.1em + rgb("#d3d8dc")),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + rgb("#d3d8dc")), table.hline(y: 1, start: 0, end: 5, stroke: 0.1em + rgb("#d3d8dc")),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat],
    ),
    // tinytable header end

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [93], [3.85],
[21.4], [6], [258], [110], [3.08],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
