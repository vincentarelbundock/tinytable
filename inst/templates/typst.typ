#show figure: set block(breakable: true)
#figure( // start figure preamble
  $TINYTABLE_TYPST_CAPTION
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = $TINYTABLE_TYPST_NHEAD;
#let nrow = $TINYTABLE_TYPST_NROW;
#let ncol = $TINYTABLE_TYPST_NCOL;


  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align after
  )
  // tinytable align-default-array after
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      set text(data.color)
      set text(data.fontsize)
      if data.indent != false { tmp = pad(left: data.indent, tmp) }
      if data.underline == true { tmp = underline(tmp) }
      if data.italic == true { tmp = emph(tmp) }
      if data.bold == true { tmp = strong(tmp) }
      if data.mono == true { tmp = math.mono(tmp) }
      if data.strikeout == true { tmp = strike(tmp) }
      tmp
    } else {
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    stroke: none,
    align: (x, y) => {
      let data = align-array.find(data => data.x.contains(x) and data.y.contains(y))
      if data != none {
        data.align
      } else {
        align-default-array.at(x)
      }
    },
    fill: (x, y) => {
      let data = fill-array.find(data => data.x.contains(x) and data.y.contains(y))
      if data != none {
        data.fill
      }
    },

    // tinytable lines after

    table.header(
      repeat: true,
    ),

    // tinytable cell content after


    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure
