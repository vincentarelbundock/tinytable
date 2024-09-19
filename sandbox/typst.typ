// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: white, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)
#show: doc => article(
  title: [`tinytable` Typst examples],
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)


```r
# Semi-complicated
tab <- tt(mtcars[1:4, 1:5], caption = "Hello World") |>
  group_tt(j = list("Group 1" = 4:5, "Group 2" = 2:3)) |>
  style_tt(j = 1:5, align = "lcccr") |>
  style_tt(i = 2, j = 1:3, strikeout = TRUE, bold = TRUE, background = "black", color = "white") |>
  style_tt(j = 1, color = "red", italic = TRUE)
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  caption: [Hello World],
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 2;
#let nrow = 4;
#let ncol = 5;

  #let fill-array = ( 
    // tinytable cell fill after
    (y: (3,), x: (0, 1, 2,), fill: black),
  )
  #let style-array = ( 
    // tinytable cell style after
    (y: (0, 1, 2, 3, 4, 5,), x: (0,), color: red, underline: false, italic: true, bold: false, mono: false, strikeout: false, fontsize: 1em, indent: false),
    (y: (3,), x: (0, 1, 2,), color: white, underline: false, italic: false, bold: true, mono: false, strikeout: true, fontsize: 1em, indent: false),
  )
  #let align-array = (
    // tinytable cell align before
  )
  #let align-default-array = ( left, center, center, center, right ,) // tinytable align-default-array here
  // tinytable align-default-array before
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 6, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 6, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 6, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 6, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 6, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 2, start: 0, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 1, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 2, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 3, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 4, end: 5, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[ ],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[Group 2],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[Group 1],
[mpg], [cyl], [disp], [hp], [drat],
    ),

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [ 93], [3.85],
[21.4], [6], [258], [110], [3.08],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Issue #139
tab <- tt(mtcars[1:10, 1:5]) |>
  group_tt(i = list("Feta" = 2, "Brie" = 6)) |>
  group_tt(j = list("First" = 2:3, "Second" = 4:5)) |>
  style_tt(1:5, align = "c", background = "blue", color = "white")
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 2;
#let nrow = 12;
#let ncol = 5;

  #let fill-array = ( 
    // tinytable cell fill after
    (y: (2, 3, 4, 5, 6,), x: (0, 1, 2, 3, 4,), fill: blue),
  )
  #let style-array = ( 
    // tinytable cell style after
    (y: (2, 3, 5, 6, 7, 8, 10, 11, 12, 13,), x: (0, 1, 2, 3, 4,), color: none, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 1em, indent: 1em),
    (y: (2, 3, 4, 5, 6,), x: (0, 1, 2, 3, 4,), color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 1em, indent: false),
  )
  #let align-array = (
    (y: (2, 3, 4, 5, 6,), x: (0, 1, 2, 3, 4,), align: center),
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 14, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 14, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 14, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 14, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 14, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 2, start: 0, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 1, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 2, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 3, end: 5, stroke: 0.05em + black),
table.hline(y: 2, start: 4, end: 5, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[ ],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[First],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[Second],
[mpg], [cyl], [disp], [hp], [drat],
    ),

    // tinytable cell content after
[21.0], [6], [160.0], [110], [3.90],
[21.0], [6], [160.0], [110], [3.90],
table.cell(colspan: 5)[Feta],
[22.8], [4], [108.0], [ 93], [3.85],
[21.4], [6], [258.0], [110], [3.08],
[18.7], [8], [360.0], [175], [3.15],
[18.1], [6], [225.0], [105], [2.76],
table.cell(colspan: 5)[Brie],
[14.3], [8], [360.0], [245], [3.21],
[24.4], [4], [146.7], [ 62], [3.69],
[22.8], [4], [140.8], [ 95], [3.92],
[19.2], [6], [167.6], [123], [3.92],
    // tinytable footer before
  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Theme striped
x <- mtcars[1:4, 1:5]
tab <- tt(x, theme = "striped")
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 4;
#let ncol = 5;

  #let fill-array = ( 
    // tinytable cell fill after
    (y: (1, 3,), x: (0, 1, 2, 3, 4,), fill: rgb("#ededed")),
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 2, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 3, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 4, end: 5, stroke: 0.05em + black),
table.hline(y: 5, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 2, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 3, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 4, end: 5, stroke: 0.05em + black),
table.hline(y: 5, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 4, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat],
    ),

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [ 93], [3.85],
[21.4], [6], [258], [110], [3.08],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Formatting
dat <- data.frame(
  w = c(143002.2092, 201399.181, 100188.3883),
  x = c(1.43402, 201.399, 0.134588),
  y = as.Date(c(897, 232, 198), origin = "1970-01-01"),
  z = c(TRUE, TRUE, FALSE))
dat <- tt(dat, digits = 2)
dat
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 3;
#let ncol = 4;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 4, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 4, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 4, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 4, stroke: 0.1em + black),
table.hline(y: 4, start: 0, end: 4, stroke: 0.1em + black),
table.hline(y: 4, start: 1, end: 4, stroke: 0.1em + black),
table.hline(y: 4, start: 2, end: 4, stroke: 0.1em + black),
table.hline(y: 4, start: 3, end: 4, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 4, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 4, stroke: 0.05em + black),
table.hline(y: 1, start: 2, end: 4, stroke: 0.05em + black),
table.hline(y: 1, start: 3, end: 4, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[w], [x], [y], [z],
    ),

    // tinytable cell content after
[143002], [  1.43], [1972-06-16], [True ],
[201399], [201.4 ], [1970-08-21], [True ],
[100188], [  0.13], [1970-07-18], [False],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# More formatting
dat <- data.frame(
  a = c("Burger", "Halloumi", "Tofu", "Beans"),
  b = c(1.43202, 201.399, 0.146188, 0.0031),
  c = c(98938272783457, 7288839482, 29111727, 93945))
tab <- tt(dat) |>
  format_tt(j = "a", sprintf = "Food: %s") |>
  format_tt(j = 2, digits = 1) |>
  format_tt(j = "c", digits = 2, num_suffix = TRUE)
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 4;
#let ncol = 3;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 3, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 3, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 3, stroke: 0.1em + black),
table.hline(y: 5, start: 0, end: 3, stroke: 0.1em + black),
table.hline(y: 5, start: 1, end: 3, stroke: 0.1em + black),
table.hline(y: 5, start: 2, end: 3, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 3, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 3, stroke: 0.05em + black),
table.hline(y: 1, start: 2, end: 3, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[a], [b], [c],
    ),

    // tinytable cell content after
[Food: Burger], [  1.432], [99T],
[Food: Halloumi], [201.399], [7.3B],
[Food: Tofu], [  0.146], [29M],
[Food: Beans], [  0.003], [94K],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Significant cell
dat <- data.frame(x = c(0.000123456789, 12.4356789))
tab <- tt(dat) |> format_tt(digits = 2, num_fmt = "significant_cell")
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 2;
#let ncol = 1;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto),
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

table.hline(y: 0, start: 0, end: 1, stroke: 0.1em + black),
table.hline(y: 3, start: 0, end: 1, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 1, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[x],
    ),

    // tinytable cell content after
[0.00012],
[12],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Missing value replacement
tab <- tt(data.frame(a = c(NA, 1, 2), b = c(3, NA, 5)))
tab <- format_tt(tab, replace = "-")
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 3;
#let ncol = 2;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto),
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

table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 2, stroke: 0.1em + black),
table.hline(y: 4, start: 0, end: 2, stroke: 0.1em + black),
table.hline(y: 4, start: 1, end: 2, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 2, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[a], [b],
    ),

    // tinytable cell content after
[-], [ 3],
[ 1], [-],
[ 2], [ 5],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Italic markdown
dat <- data.frame(markdown = c("This is _italic_ text."))
tab <- tt(dat) |>
  format_tt(j = 1, markdown = TRUE) |>
  style_tt(j = 1, align = "c")
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 1;
#let ncol = 1;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  #let align-default-array = ( center ,) // tinytable align-default-array here
  // tinytable align-default-array before
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto),
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

table.hline(y: 0, start: 0, end: 1, stroke: 0.1em + black),
table.hline(y: 2, start: 0, end: 1, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 1, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[markdown],
    ),

    // tinytable cell content after
[This is _italic_ text.],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Font size
dat <- tt(x) |> style_tt(j = "mpg|hp|qsec", fontsize = 1.5)
dat
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 4;
#let ncol = 5;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
    (y: (0, 1, 2, 3, 4,), x: (0, 3,), color: none, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 1.5em, indent: false),
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 2, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 3, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 4, end: 5, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat],
    ),

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [ 93], [3.85],
[21.4], [6], [258], [110], [3.08],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
tt(x) |> style_tt(i = 2, j = 2:3, line = "b", line_color = "green")
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 4;
#let ncol = 5;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto),
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

table.hline(y: 3, start: 1, end: 3, stroke: 0.1em + green),
table.hline(y: 3, start: 2, end: 3, stroke: 0.1em + green),
table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 2, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 3, end: 5, stroke: 0.05em + black),
table.hline(y: 1, start: 4, end: 5, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat],
    ),

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [ 93], [3.85],
[21.4], [6], [258], [110], [3.08],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# No headers
k <- x
colnames(k) <- NULL
k <- tt(k)
k |> style_tt(i = 2, j = 2:3, line = "b", line_color = "green")
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 0;
#let nrow = 4;
#let ncol = 5;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto),
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

table.hline(y: 3, start: 1, end: 3, stroke: 0.1em + green),
table.hline(y: 3, start: 2, end: 3, stroke: 0.1em + green),
table.hline(y: 1, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 1, start: 4, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 0, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 1, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 2, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 3, end: 5, stroke: 0.1em + black),
table.hline(y: 5, start: 4, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
    ),

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [ 93], [3.85],
[21.4], [6], [258], [110], [3.08],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Group rows
dat <- mtcars[1:9, 1:8]
dat <- tt(dat) |>
  group_tt(i = list(
    "I like (fake) hamburgers" = 3,
    "She prefers halloumi" = 4,
    "They love tofu" = 7))
dat
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 12;
#let ncol = 8;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
    (y: (1, 2, 4, 6, 7, 8, 10, 11, 12,), x: (0, 1, 2, 3, 4, 5, 6, 7,), color: none, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 1em, indent: 1em),
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto, auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 5, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 6, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 7, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 0, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 1, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 2, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 3, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 4, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 5, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 6, end: 8, stroke: 0.1em + black),
table.hline(y: 13, start: 7, end: 8, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 8, stroke: 0.05em + black),
table.hline(y: 1, start: 1, end: 8, stroke: 0.05em + black),
table.hline(y: 1, start: 2, end: 8, stroke: 0.05em + black),
table.hline(y: 1, start: 3, end: 8, stroke: 0.05em + black),
table.hline(y: 1, start: 4, end: 8, stroke: 0.05em + black),
table.hline(y: 1, start: 5, end: 8, stroke: 0.05em + black),
table.hline(y: 1, start: 6, end: 8, stroke: 0.05em + black),
table.hline(y: 1, start: 7, end: 8, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat], [wt], [qsec], [vs],
    ),

    // tinytable cell content after
[21.0], [6], [160.0], [110], [3.90], [2.620], [16.46], [0],
[21.0], [6], [160.0], [110], [3.90], [2.875], [17.02], [0],
table.cell(colspan: 8)[I like (fake) hamburgers],
[22.8], [4], [108.0], [ 93], [3.85], [2.320], [18.61], [1],
table.cell(colspan: 8)[She prefers halloumi],
[21.4], [6], [258.0], [110], [3.08], [3.215], [19.44], [1],
[18.7], [8], [360.0], [175], [3.15], [3.440], [17.02], [0],
[18.1], [6], [225.0], [105], [2.76], [3.460], [20.22], [1],
table.cell(colspan: 8)[They love tofu],
[14.3], [8], [360.0], [245], [3.21], [3.570], [15.84], [0],
[24.4], [4], [146.7], [ 62], [3.69], [3.190], [20.00], [1],
[22.8], [4], [140.8], [ 95], [3.92], [3.150], [22.90], [1],
    // tinytable footer before
  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Group columns
dat <- mtcars[1:9, 1:8]
tab <- tt(dat) |>
  group_tt(
    j = list(
      "Hamburgers" = 1:3,
      "Halloumi" = 4:5,
      "Tofu" = 7))
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 2;
#let nrow = 9;
#let ncol = 8;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    column-gutter: 5pt,
    columns: (auto, auto, auto, auto, auto, auto, auto, auto),
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

table.hline(y: 0, start: 0, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 1, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 2, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 3, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 4, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 5, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 6, end: 8, stroke: 0.1em + black),
table.hline(y: 0, start: 7, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 0, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 1, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 2, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 3, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 4, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 5, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 6, end: 8, stroke: 0.1em + black),
table.hline(y: 11, start: 7, end: 8, stroke: 0.1em + black),
table.hline(y: 2, start: 0, end: 8, stroke: 0.05em + black),
table.hline(y: 2, start: 1, end: 8, stroke: 0.05em + black),
table.hline(y: 2, start: 2, end: 8, stroke: 0.05em + black),
table.hline(y: 2, start: 3, end: 8, stroke: 0.05em + black),
table.hline(y: 2, start: 4, end: 8, stroke: 0.05em + black),
table.hline(y: 2, start: 5, end: 8, stroke: 0.05em + black),
table.hline(y: 2, start: 6, end: 8, stroke: 0.05em + black),
table.hline(y: 2, start: 7, end: 8, stroke: 0.05em + black),
    // tinytable lines before

    table.header(
      repeat: true,
table.cell(stroke: (bottom: .05em + black), colspan: 3, align: center)[Hamburgers],table.cell(stroke: (bottom: .05em + black), colspan: 2, align: center)[Halloumi],[ ],table.cell(stroke: (bottom: .05em + black), colspan: 1, align: center)[Tofu],[ ],
[mpg], [cyl], [disp], [hp], [drat], [wt], [qsec], [vs],
    ),

    // tinytable cell content after
[21.0], [6], [160.0], [110], [3.90], [2.620], [16.46], [0],
[21.0], [6], [160.0], [110], [3.90], [2.875], [17.02], [0],
[22.8], [4], [108.0], [ 93], [3.85], [2.320], [18.61], [1],
[21.4], [6], [258.0], [110], [3.08], [3.215], [19.44], [1],
[18.7], [8], [360.0], [175], [3.15], [3.440], [17.02], [0],
[18.1], [6], [225.0], [105], [2.76], [3.460], [20.22], [1],
[14.3], [8], [360.0], [245], [3.21], [3.570], [15.84], [0],
[24.4], [4], [146.7], [ 62], [3.69], [3.190], [20.00], [1],
[22.8], [4], [140.8], [ 95], [3.92], [3.150], [22.90], [1],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure
```r
# Theme grid
tab <- tt(x, theme = "grid")
tab
```

#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 4;
#let ncol = 5;

  #let fill-array = ( 
    // tinytable cell fill after
  )
  #let style-array = ( 
    // tinytable cell style after
  )
  #let align-array = (
    // tinytable cell align before
  )
  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x.contains(it.x) and data.y.contains(it.y))
    if data != none {
      if data.fontsize != none { tmp = text(size: data.fontsize, tmp) }
      if data.color != none { tmp = text(fill: data.color, tmp) }
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
    columns: (auto, auto, auto, auto, auto),
    stroke: (paint: black),
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

    // tinytable lines before

    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat],
    ),

    // tinytable cell content after
[21.0], [6], [160], [110], [3.90],
[21.0], [6], [160], [110], [3.90],
[22.8], [4], [108], [ 93], [3.85],
[21.4], [6], [258], [110], [3.08],

    // tinytable footer before

  ) // end table

  ]) // end align

] // end block
) // end figure




