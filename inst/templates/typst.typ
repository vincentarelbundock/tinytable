#let nhead = $TINYTABLE_TYPST_NHEAD;
#let nrow = $TINYTABLE_TYPST_NROW;
#let ncol = $TINYTABLE_TYPST_NCOL;

// start figure preamble
#figure(
  $TINYTABLE_TYPST_CAPTION
  kind: "tinytable",
  supplement: none,
// end figure preamble

  #show table.cell.where(x: 0, y: 1): set text(orange)

  table(
    columns: ncol,
    align: left + horizon,
    stroke: none,
    fill: (x, y) => {
      let data = fillarray.find(data => data.x == x and data.y == y)
      if data != none {
        data.fill
      }
    },

    // tinytable lines before
    // tinytable cell style before
    // tinytable cell content after

  ) // end table

) // end figure
