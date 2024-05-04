
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
    // TODO: insert fill at top because it overrides lower calls
    // (x: 0, y: 1, fill: red), 
    // (x: 0, y: 1, fill: yellow), 
    // (x: 2, y: 1, fill: blue) 
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x == it.x and data.y == it.y)
    if data != none {
      if (data.color == none) { data.color = black }
      set text(data.color)
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

    // tinytable lines before
    // tinytable cell style before
    // tinytable cell content after

  ) // end table

) // end figure
]
