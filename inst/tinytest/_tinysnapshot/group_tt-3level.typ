#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 4;
#let nrow = 3;
#let ncol = 5;

  #let style-array = ( 
    // tinytable cell style after
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
      if data.indent != none { tmp = pad(left: data.indent, tmp) }
      if data.underline != none { tmp = underline(tmp) }
      if data.italic != none { tmp = emph(tmp) }
      if data.bold != none { tmp = strong(tmp) }
      if data.mono != none { tmp = math.mono(tmp) }
      if data.strikeout != none { tmp = strike(tmp) }
      tmp
    } else {
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    column-gutter: 5pt,
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    align: (x, y) => {
      let data = style-array.find(data => data.x.contains(x) and data.y.contains(y))
      if data != none and data.align != none {
        data.align
      } else {
        left
      }
    },
    fill: (x, y) => {
      let data = style-array.find(data => data.x.contains(x) and data.y.contains(y))
      if data != none and data.background != none { 
            data.background
      }
    },

table.hline(y: 4, start: 0, end: 5, stroke: 0.05em + black),
table.hline(y: 7, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
table.cell(stroke: (bottom: .05em + black), colspan: 3, align: center)[e],table.cell(stroke: (bottom: .05em + black), colspan: 1, align: center)[f],[ ],
table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[c],table.cell(stroke: (bottom: .05em + black), colspan: 3, align: center)[d],
[ ],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[a],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[b],
[mpg], [cyl], [disp], [hp], [drat],
    ),

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [ 93], [3.85],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
