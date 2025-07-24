#show figure: set block(breakable: true)
#figure( // start preamble figure
  
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
  )

  #let style-array = ( 
    // tinytable cell style after
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, left, ) // tinytable align-default-array here
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
    columns: (auto, auto, auto, auto, auto, auto, auto, auto),
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
 table.hline(y: 2, start: 0, end: 8, stroke: 0.05em + black),
 table.hline(y: 11, start: 0, end: 8, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 8, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
table.cell(stroke: (bottom: .05em + black), colspan: 3, align: center)[Hamburgers],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[Halloumi],[ ],table.cell(stroke: (bottom: .05em + black), colspan: 1, align: center)[Tofu],[ ],
[mpg], [cyl], [disp], [hp], [drat], [wt], [qsec], [vs],
    ),
    // tinytable header end

    // tinytable cell content after
[21.0], [6], [160.0], [110], [3.90], [2.620], [16.46], [0],
[21.0], [6], [160.0], [110], [3.90], [2.875], [17.02], [0],
[22.8], [4], [108.0], [93], [3.85], [2.320], [18.61], [1],
[21.4], [6], [258.0], [110], [3.08], [3.215], [19.44], [1],
[18.7], [8], [360.0], [175], [3.15], [3.440], [17.02], [0],
[18.1], [6], [225.0], [105], [2.76], [3.460], [20.22], [1],
[14.3], [8], [360.0], [245], [3.21], [3.570], [15.84], [0],
[24.4], [4], [146.7], [62], [3.69], [3.190], [20.00], [1],
[22.8], [4], [140.8], [95], [3.92], [3.150], [22.90], [1],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
