
#import "@preview/tablex:0.0.8": tablex, rowspanx, colspanx

#tablex(
  columns: 4,
  align: left + horizon,
  auto-vlines: false,
  header-rows: 1,

  map-cells: cell => {

    let j = (1,);
    if (j.contains(cell.x)) { cell.align = center };

    let i = (2,4);
    let j = (1,);
    if (i.contains(cell.y) and j.contains(cell.x)) { cell.fill = black };

    let i = (2,4);
    let j = (1,);
    if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = { set text(white); cell.content } }

    let i = (0,);
    if (i.contains(cell.y)) { cell.content = { emph(cell.content) } }

    let i = (1,);
    if (i.contains(cell.y)) { cell.content = { strong(cell.content) } }
    return cell;

  },


  [Username], [Location], [Height], [Score],
  [John], [Second St.], [180 cm], [5],
  [Wally], [Third Av.], [160 cm], [10],
  [Jason], [Some St.], [150 cm], [15],
  [Robert], [123 Av.], [190 cm], [20],
  [Other], [Unknown St.], [170 cm], [25],
)