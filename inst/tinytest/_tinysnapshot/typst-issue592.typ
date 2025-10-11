#show figure: set block(breakable: true)
#figure( // start preamble figure
  
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "6_0": 0, "11_0": 0, "1_1": 1, "2_1": 1, "3_1": 1, "4_1": 1, "5_1": 1, "6_1": 1, "7_1": 1, "8_1": 1, "9_1": 1, "10_1": 1, "11_1": 1, "12_1": 1, "13_1": 1, "14_1": 1, "15_1": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center + horizon,),
    (align: horizon,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, ) // tinytable align-default-array here
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
    columns: (auto, auto, auto, auto, auto, auto, auto),
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
 table.hline(y: 1, start: 0, end: 7, stroke: 0.1em + black),
 table.hline(y: 16, start: 0, end: 7, stroke: 0.1em + black),

 table.hline(y: 0, start: 0, end: 7, stroke: 0.1em + black),
 table.hline(y: 1, start: 0, end: 7, stroke: 0.1em + black),
 table.hline(y: 2, start: 1, end: 7, stroke: 0.1em + black),
 table.hline(y: 3, start: 1, end: 7, stroke: 0.1em + black),
 table.hline(y: 4, start: 1, end: 2, stroke: 0.1em + black), table.hline(y: 4, start: 3, end: 7, stroke: 0.1em + black),
 table.hline(y: 5, start: 1, end: 2, stroke: 0.1em + black), table.hline(y: 5, start: 3, end: 7, stroke: 0.1em + black),
 table.hline(y: 6, start: 0, end: 7, stroke: 0.1em + black),
 table.hline(y: 7, start: 1, end: 7, stroke: 0.1em + black),
 table.hline(y: 8, start: 1, end: 7, stroke: 0.1em + black),
 table.hline(y: 9, start: 1, end: 2, stroke: 0.1em + black), table.hline(y: 9, start: 3, end: 7, stroke: 0.1em + black),
 table.hline(y: 10, start: 1, end: 2, stroke: 0.1em + black), table.hline(y: 10, start: 3, end: 7, stroke: 0.1em + black),
 table.hline(y: 11, start: 0, end: 7, stroke: 0.1em + black),
 table.hline(y: 12, start: 1, end: 7, stroke: 0.1em + black),
 table.hline(y: 13, start: 1, end: 7, stroke: 0.1em + black),
 table.hline(y: 14, start: 1, end: 2, stroke: 0.1em + black), table.hline(y: 14, start: 3, end: 7, stroke: 0.1em + black),
 table.hline(y: 15, start: 1, end: 2, stroke: 0.1em + black), table.hline(y: 15, start: 3, end: 7, stroke: 0.1em + black),
 table.hline(y: 16, start: 2, end: 3, stroke: 0.1em + black),
 table.vline(x: 0, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 0, start: 0, end: 2, stroke: 0.1em + black),
 table.vline(x: 0, start: 1, end: 7, stroke: 0.1em + black),
 table.vline(x: 0, start: 6, end: 12, stroke: 0.1em + black),
 table.vline(x: 0, start: 11, end: 16, stroke: 0.1em + black),
 table.vline(x: 1, start: 0, end: 16, stroke: 0.1em + black),
 table.vline(x: 2, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 3, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 4, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 5, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 6, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 1, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 2, start: 0, end: 1, stroke: 0.1em + black),
 table.vline(x: 3, start: 0, end: 16, stroke: 0.1em + black),
 table.vline(x: 4, start: 0, end: 16, stroke: 0.1em + black),
 table.vline(x: 5, start: 0, end: 16, stroke: 0.1em + black),
 table.vline(x: 6, start: 0, end: 16, stroke: 0.1em + black),
 table.vline(x: 7, start: 0, end: 16, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[metric], [tier], [role], [Overall], [state1], [state2], [state3],
    ),
    // tinytable header end

    // tinytable cell content after
table.cell(rowspan: 5)[Adoption], [1], [], [90.9%], [90.5%], [95.7%], [91.5%],
[2], [x], [87.9%], [91%], [75%], [85.9%],
table.cell(rowspan: 3)[3], [x], [90.6%], [91%], [ ], [95.4%],
[y], [90.6%], [92.3%], [ ], [100%],
[z], [100%], [100%], [ ], [ ],
table.cell(rowspan: 5)[NPS], [1], [], [19], [24.2], [11.5], [23.8],
[2], [x], [39.9], [40.3], [45.7], [44.7],
table.cell(rowspan: 3)[3], [x], [47.2], [51.4], [ ], [70.8],
[y], [14.6], [11.5], [ ], [22.2],
[z], [100], [100], [ ], [ ],
table.cell(rowspan: 5)[Overall Satisfaction], [1], [], [80.7%], [82.4%], [79.7%], [82.4%],
[2], [x], [90.4%], [89.4%], [94.4%], [88.1%],
table.cell(rowspan: 3)[3], [x], [80.9%], [78.4%], [ ], [55.6%],
[y], [91.8%], [91.6%], [ ], [98.4%],
[z], [100%], [100%], [ ], [ ],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
