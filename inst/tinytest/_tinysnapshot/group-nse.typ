#show figure: set block(breakable: true)
#figure( // start preamble figure
  
  kind: "tinytable",
  supplement: "Table", // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
  )

  #let style-array = ( 
    // tinytable cell style after
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto, auto, auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 11, stroke: 0.05em + black),
 table.hline(y: 35, start: 0, end: 11, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 11, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat], [wt], [qsec], [vs], [am], [gear], [carb],
    ),
    // tinytable header end

    // tinytable cell content after
table.cell(colspan: 11)[0],
[21.4], [6], [258.0], [110], [3.08], [3.215], [19.44], [1], [0], [3], [1],
[18.7], [8], [360.0], [175], [3.15], [3.440], [17.02], [0], [0], [3], [2],
[18.1], [6], [225.0], [105], [2.76], [3.460], [20.22], [1], [0], [3], [1],
[14.3], [8], [360.0], [245], [3.21], [3.570], [15.84], [0], [0], [3], [4],
[24.4], [4], [146.7], [62], [3.69], [3.190], [20.00], [1], [0], [4], [2],
[22.8], [4], [140.8], [95], [3.92], [3.150], [22.90], [1], [0], [4], [2],
[19.2], [6], [167.6], [123], [3.92], [3.440], [18.30], [1], [0], [4], [4],
[17.8], [6], [167.6], [123], [3.92], [3.440], [18.90], [1], [0], [4], [4],
[16.4], [8], [275.8], [180], [3.07], [4.070], [17.40], [0], [0], [3], [3],
[17.3], [8], [275.8], [180], [3.07], [3.730], [17.60], [0], [0], [3], [3],
[15.2], [8], [275.8], [180], [3.07], [3.780], [18.00], [0], [0], [3], [3],
[10.4], [8], [472.0], [205], [2.93], [5.250], [17.98], [0], [0], [3], [4],
[10.4], [8], [460.0], [215], [3.00], [5.424], [17.82], [0], [0], [3], [4],
[14.7], [8], [440.0], [230], [3.23], [5.345], [17.42], [0], [0], [3], [4],
[21.5], [4], [120.1], [97], [3.70], [2.465], [20.01], [1], [0], [3], [1],
[15.5], [8], [318.0], [150], [2.76], [3.520], [16.87], [0], [0], [3], [2],
[15.2], [8], [304.0], [150], [3.15], [3.435], [17.30], [0], [0], [3], [2],
[13.3], [8], [350.0], [245], [3.73], [3.840], [15.41], [0], [0], [3], [4],
[19.2], [8], [400.0], [175], [3.08], [3.845], [17.05], [0], [0], [3], [2],
table.cell(colspan: 11)[1],
[21.0], [6], [160.0], [110], [3.90], [2.620], [16.46], [0], [1], [4], [4],
[21.0], [6], [160.0], [110], [3.90], [2.875], [17.02], [0], [1], [4], [4],
[22.8], [4], [108.0], [93], [3.85], [2.320], [18.61], [1], [1], [4], [1],
[32.4], [4], [78.7], [66], [4.08], [2.200], [19.47], [1], [1], [4], [1],
[30.4], [4], [75.7], [52], [4.93], [1.615], [18.52], [1], [1], [4], [2],
[33.9], [4], [71.1], [65], [4.22], [1.835], [19.90], [1], [1], [4], [1],
[27.3], [4], [79.0], [66], [4.08], [1.935], [18.90], [1], [1], [4], [1],
[26.0], [4], [120.3], [91], [4.43], [2.140], [16.70], [0], [1], [5], [2],
[30.4], [4], [95.1], [113], [3.77], [1.513], [16.90], [1], [1], [5], [2],
[15.8], [8], [351.0], [264], [4.22], [3.170], [14.50], [0], [1], [5], [4],
[19.7], [6], [145.0], [175], [3.62], [2.770], [15.50], [0], [1], [5], [6],
[15.0], [8], [301.0], [335], [3.54], [3.570], [14.60], [0], [1], [5], [8],
[21.4], [4], [121.0], [109], [4.11], [2.780], [18.60], [1], [1], [4], [2],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
) // end figure 
