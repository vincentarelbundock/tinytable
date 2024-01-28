#import "@preview/tablex:0.0.8": tablex, hlinex, vlinex
#let nhead = $TINYTABLE_TYPST_NHEAD;
#let nrow = $TINYTABLE_TYPST_NROW;
#let ncol = $TINYTABLE_TYPST_NCOL;

#tablex(
  columns: ncol,
  header-rows: nhead,
  align: left + horizon,
  auto-lines: false,

  // tinytable lines before

  map-cells: cell => {

    // tinytable cell style before

    return cell;
  },

  // tinytable cell content before
)
