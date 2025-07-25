#show figure: set block(breakable: true)
#figure( // start preamble figure
  
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "3_0": 0, "2_0": 1, "4_0": 1, "5_0": 2, "6_0": 2, "1_1": 3, "0_1": 3, "2_1": 3, "3_1": 3, "4_1": 3, "5_1": 3, "6_1": 3
  )

  #let style-array = ( 
    // tinytable cell style after
    (bold: true,),
    (bold: true, indent: 1em,),
    (indent: 1em,),
    (italic: true,),
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
    column-gutter: 5pt,
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
 table.hline(y: 2, start: 0, end: 4, stroke: 0.05em + black),
 table.hline(y: 7, start: 0, end: 4, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 4, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[foo],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[bar],
[mpg], [cyl], [disp], [hp],
    ),
    // tinytable header end

    // tinytable cell content after
[21.000], [6], [160], [110],
table.cell(colspan: 4)[hello world],
[21.000], [6], [160], [110],
[22.800], [4], [108], [93],
[21.400], [6], [258], [110],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
