#show figure: set block(breakable: true)
#figure( // start figure preamble
  $TINYTABLE_TYPST_CAPTION
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = $TINYTABLE_TYPST_NHEAD;
#let nrow = $TINYTABLE_TYPST_NROW;
#let ncol = $TINYTABLE_TYPST_NCOL;

  #let style-array = ( 
    // tinytable cell style after
  )

  // tinytable align-default-array before
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

    // tinytable lines before

    table.header(
      repeat: true,
    ),

    // tinytable cell content after

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure
