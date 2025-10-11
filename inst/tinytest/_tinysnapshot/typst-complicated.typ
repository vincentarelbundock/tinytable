#show figure: set block(breakable: true)
#figure( // start preamble figure
  caption: text([Hello World]),
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "4_1": 0, "5_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "4_2": 0, "5_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "0_4": 1, "1_4": 1, "2_4": 1, "3_4": 1, "4_4": 1, "5_4": 1, "3_0": 2, "3_1": 3, "3_2": 3, "0_0": 4, "1_0": 4, "2_0": 4, "4_0": 4, "5_0": 4
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: right,),
    (bold: true, italic: true, strikeout: true, color: rgb("#FF0000"), background: black, align: left,),
    (bold: true, strikeout: true, color: white, background: black, align: center,),
    (italic: true, color: rgb("#FF0000"), align: left,),
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
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
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
 table.hline(y: 1, start: 1, end: 3, stroke: 0.05em + black), table.hline(y: 1, start: 3, end: 5, stroke: 0.05em + black),
 table.hline(y: 2, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 6, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], table.cell(colspan: 2, align: center)[Group 2], table.cell(colspan: 2, align: center)[Group 1],
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
