

<p align="center">
<img src="man/figures/IttyBittyTable_logo.svg" height = "250" class = "center">
</p>

<br> <!-- badges: start --> <!-- badges: end -->

## What?

`IttyBittyTable` is a small but powerful `R` package to draw HTML,
LaTeX, PDF, Markdown, and Typst tables. The interface is minimalist, but
it gives users direct and convenient access to powerful frameworks to
create endlessly customizable tables:

-   `tabularray` for LaTeX and PDF
-   Bootstrap for HTML.

## Why?

The design philosophy of this package rests on three pillars.

1.  *Data is separate from style*.

The code that this package creates keeps the content of a table separate
from the style sheet that applies to its cells. This is in contrast to
other `R` packages that modify the actual text in each cell to style it.
Keeping data and style separate allows `tidytable` to create
human-readable files which are easy to edit, debug, and extend. It also
enables developers to keep a tidy code base, with minimal use of messy
regular expressions.

1.  *Flexibility!*

Users’ needs are extremely varied, and a table-drawing package must be
flexible enough to accomodate different ideas. To achieve this,
`IttyBittyTable` builds on battle-tested and versatile frameworks like
`Bootstrap` (HTML) and `tabularray` (LaTeX).

1.  [*Lightweight is the right weight.*](https://www.tinyverse.org/)

Some of the most popular table-drawing packages in the `R` ecosystem are
heavy. For instance, `kableExtra` imports 66 `R` dependencies, `gt` 65,
`DT` 43, and `huxtable` 29. In contrast, `IttyBittyTable` imports zero
3rd party `R` package by default.

`IttyBittyTable` is not only lightweight in terms of dependencies, it is
also “small” along several other dimensions:

-   The user interface is simple, streamlined, consistent, uncluttered.
-   The `IttyBittyTable` code base is very small and easy to maintain.
-   `IttyBittyTable` is a very thin wrapper around incredibly powerful
    frameworks: Bootstrap for HTML and `tabularray` for LaTeX.
-   This package is free. Tiny tables for a tiny price!

## Installation

You can install the development version of IttyBittyTable from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("vincentarelbundock/IttyBittyTable")
```

## Get started

TODO
