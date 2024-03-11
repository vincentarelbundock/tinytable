

<p align="center">
<img src="man/figures/gallery/tinytable_gallery.gif" height = "250" class = "center">
<br> <!-- badges: start -->
<a href = "https://github.com/vincentarelbundock/tinytable/blob/main/LICENSE.md" target = "_blank"><img src="https://img.shields.io/badge/license-GPLv3-blue"></a>
<a href = "https://vincentarelbundock.github.io/tinytable/" target = "_blank"><img src="https://img.shields.io/static/v1?label=Website&message=Visit&color=blue"></a>
<a href = "https://cran.r-project.org/package=tinytable" target = "_blank"><img src="https://cranlogs.r-pkg.org/badges/tinytable"></a>
<!-- badges: end -->
</p>

## What?

`tinytable` is a small but powerful `R` package to draw beautiful tables
in a variety of formats: HTML, LaTeX, Word, PDF, PNG, Markdown, and
Typst. The user interface is minimalist and easy to learn, while giving
users access to powerful frameworks to create endlessly customizable
tables.

<https://vincentarelbundock.github.io/tinytable/>

## Why?

There are already many excellent table-drawing packages in the `R`
ecosystem. Why release a new one? As [the maintainer of
`modelsummary`](https://modelsummary.com), I needed a table-drawing
package which was:

-   *Simple*: Streamlined, consistent, and uncluttered user interface,
    with few functions to learn.
-   *Flexible*: Expressive frameworks to customize tables in HTML and
    LaTeX formats.[1]
-   *Zero-dependency*: Avoid importing any other `R` package.[2]
-   *Concise*: Draw beautiful tables without typing a lot of code.
-   *Safe*: User inputs are checked thoroughly, and informative errors
    are returned early.
-   *Maintainable*: A small code base which does not rely on too many
    complex regular expressions.
-   *Readable*: HTML and LaTeX code should be human-readable and
    editable.
-   *Free*: This package will always be free. Tiny tables for a tiny
    price!

To achieve these goals, the design philosophy of `tinytable` rests on
three pillars:

1.  *Data is separate from style.* The code that this package creates
    keeps the content of a table separate from the style sheet that
    applies to its cells. This is in contrast to other `R` packages that
    modify the actual text in each cell to style it. Keeping data and
    style separate allows `tinytable` to create human-readable files
    which are easy to edit, debug, and extend. It also enables
    developers to keep a simpler code base, with minimal use of messy
    regular expressions.

2.  *Flexibility.* Users’ needs are extremely varied, and a
    table-drawing package must be flexible enough to accomodate
    different ideas. To achieve this, `tinytable` builds on
    battle-tested and versatile frameworks like `Bootstrap` for HTML and
    `tabularray` for LaTeX.

3.  [*Lightweight is the right weight.*](https://www.tinyverse.org/)
    Some of the most popular table-drawing packages in the `R` ecosystem
    are very heavy: A single `library()` call can sometimes load upwards
    of 65 `R` packages. In contrast, `tinytable` imports zero 3rd party
    `R` package by default.

## Installation

Install the stable version from CRAN:

``` r
install.packages("tinytable")
```

`tinytable` is a relatively new package with rapid development. If you
want to benefit from the latest features—showcased on the package
website—you may want to install the development version from Github.

``` r
library(remotes)
install_github("vincentarelbundock/tinytable")
```

Restart `R` completely for the installation to take effect.

## Get started

-   [Tutorial
    (HTML)](https://vincentarelbundock.github.io/tinytable/vignettes/tutorial.html)
-   [Tutorial
    (PDF)](https://vincentarelbundock.github.io/tinytable/vignettes/tutorial.pdf)

[1] Other formats like Markdown and Typst are also available, but less
flexible.

[2] Some extra packages can be imported to access specific
functionality, such as integration with Quarto, inserting `ggplot2`
objects as inline plots, and saving tables to PNG images or PDF
documents.
