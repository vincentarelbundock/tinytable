
#[
#let nhead = $TINYTABLE_TYPST_NHEAD;
#let nrow = $TINYTABLE_TYPST_NROW;
#let ncol = $TINYTABLE_TYPST_NCOL;

// start figure preamble
#figure(
  $TINYTABLE_TYPST_CAPTION
  kind: "tinytable",
  supplement: none,
// end figure preamble

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x == it.x and data.y == it.y)
    if data != none {
      set text(data.color)
      set text(data.fontsize)
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

  table(
    columns: ncol,
    align: left + horizon,
    stroke: none,
    fill: (x, y) => {
      let data = fill-array.find(data => data.x == x and data.y == y)
      if data != none {
        data.fill
      }
    },

    // tinytable lines after
    // tinytable cell content after

  ) // end table

) // end figure
]
