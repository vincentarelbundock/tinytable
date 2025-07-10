#show figure: set block(breakable: true)
#figure( // start figure preamble
  $TINYTABLE_TYPST_CAPTION
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = $TINYTABLE_TYPST_NHEAD;
#let nrow = $TINYTABLE_TYPST_NROW;
#let ncol = $TINYTABLE_TYPST_NCOL;

  #let style-dict = (
    // tinytable style-dict after
  )

  #let style-array = ( 
    // tinytable cell style after
  )

  // tinytable align-default-array before
  #show table.cell: it => {
    if style-array.len() == 0 {
      it 
    } else {
      let tmp = it
      let key = str(it.y) + "_" + str(it.x)
      let style-index = if key in style-dict { style-dict.at(key) } else { none }
      if style-index != none {
        let style = style-array.at(style-index)
        if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
        if ("color" in style) { tmp = text(fill: style.color, tmp) }
        if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
        if ("underline" in style) { tmp = underline(tmp) }
        if ("italic" in style) { tmp = emph(tmp) }
        if ("bold" in style) { tmp = strong(tmp) }
        if ("mono" in style) { tmp = math.mono(tmp) }
        if ("strikeout" in style) { tmp = strike(tmp) }
      }
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    stroke: none,
    align: (x, y) => {
      let key = str(y) + "_" + str(x)
      let style-index = if key in style-dict { style-dict.at(key) } else { none }
      if style-index != none {
        let style = style-array.at(style-index)
        if ("align" in style) {
          style.align
        } else {
          left
        }
      } else {
        left
      }
    },
    fill: (x, y) => {
      let key = str(y) + "_" + str(x)
      let style-index = if key in style-dict { style-dict.at(key) } else { none }
      if style-index != none {
        let style = style-array.at(style-index)
        if ("background" in style) {
          style.background
        }
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
