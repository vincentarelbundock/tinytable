
#import "@preview/tablex:0.0.8": tablex, hlinex, vlinex
#let nrows = 6;
#let nhead = 1;

#tablex(
  columns: 4,
  align: left + horizon,
  auto-lines: false,
  header-rows: 1,

  // lines
  hlinex(y: 0, stroke: black + .1em),
  hlinex(y: nhead, stroke: black + .05em),
  hlinex(y: nrows, stroke: black + .1em),
  hlinex(y: 6, stroke: black + .1em),
  hlinex(start: 0, end: 3, y: 4, stroke: rgb("#239dad") + .2em),
  vlinex(x: 2, stroke: yellow + .5em),

  map-cells: cell => {

    // align
    let j = (1,);
    if (j.contains(cell.x)) { cell.align = center };

    // background
    let i = (2,4);
    let j = (1,);
    if (i.contains(cell.y) and j.contains(cell.x)) { cell.fill = olive };

    // color
    let i = (2,4);
    let j = (1,);
    if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = { set text(white); cell.content } }

    // italic
    let i = (0,);
    if (i.contains(cell.y)) { cell.content = { emph(cell.content) } }

    // underline
    let i = (3,4,);
    let j = (3,);
    if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = { underline(cell.content) } }

    // underline
    let i = (3,4,);
    let j = (3,);
    if (i.contains(cell.y) and j.contains(cell.x)) { cell.content = { strike(cell.content) } }

    // bold
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