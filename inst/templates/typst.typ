#import "@preview/tablex:0.0.8": tablex, hlinex, vlinex, colspanx
#let nhead = $TINYTABLE_TYPST_NHEAD;
#let nrow = $TINYTABLE_TYPST_NROW;
#let ncol = $TINYTABLE_TYPST_NCOL;

#figure(
  $TINYTABLE_TYPST_CAPTION
  kind: "tinytable",
  supplement: none,
  tablex(
    columns: ncol,
    header-rows: nhead,
    align: left + horizon,
    auto-lines: false,

    // tinytable lines before

    map-cells: cell => {
      // tinytable cell style before
      return cell;
    },

    // tinytable cell content after
  ) // end tablex
) // end figure

