#show figure: set block(breakable: true)
#figure( // start preamble figure
  
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "3_0": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "12_0": 1, "13_0": 1, "2_0": 2, "4_0": 2, "5_0": 2, "6_0": 2
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center, color: white, background: rgb("#0000FF"),),
    (indent: 1em,),
    (indent: 1em, align: center, color: white, background: rgb("#0000FF"),),
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
 table.hline(y: 2, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 14, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[First],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[Second],
[mpg], [cyl], [disp], [hp], [drat],
    ),
    // tinytable header end

    // tinytable cell content after
[21.0], [6], [160.0], [110], [3.90],
table.cell(colspan: 5)[Feta],
[21.0], [6], [160.0], [110], [3.90],
[22.8], [4], [108.0], [93], [3.85],
[21.4], [6], [258.0], [110], [3.08],
[18.7], [8], [360.0], [175], [3.15],
table.cell(colspan: 5)[Brie],
[18.1], [6], [225.0], [105], [2.76],
[14.3], [8], [360.0], [245], [3.21],
[24.4], [4], [146.7], [62], [3.69],
[22.8], [4], [140.8], [95], [3.92],
[19.2], [6], [167.6], [123], [3.92],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
