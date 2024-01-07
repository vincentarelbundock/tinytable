
<p align="center">
<img src="man/figures/tinytable_logo.png" height = "250" class = "center">
</p>

<br> <!-- badges: start --> <!-- badges: end -->

## What?

`tinytable` is a small but powerful `R` package to draw LaTeX/PDF, HTML,
Markdown, and Typst tables. The interface is minimalist, but it gives
users direct access to powerful frameworks to create endlessly
customizable tables: `tabularray` for LaTeX/PDF and Bootstrap for HTML.

## Why?

The design philosophy of this package rests on three pillars.

First, *data is separate from style*. The code that `tinytable` creates
keeps the data (characters and numbers in a table) separate from the
style that should be applied to each cell. This brings important
benefits: the files produced by the package are human-readable, which
makes them easy to edit and customize. Moreover, keeping data separate
from style can be very useful when diagnosing problems.

Second, *tables should be deeply customizable.* Users’ needs are
incredibly varied, and a table drawing package should be flexible enough
to accomodate many ideas. To achieve this, `tinytable` builds on the
work of existing frameworks for table creating in HTML and LaTeX
formats.

Third, [*lightweight is the right weight.*](https://www.tinyverse.org/)
Some of the most popular table-drawing packages in the `R` ecosystem are
heavy. For instance, `kableExtra` imports 66 dependencies, `gt` 65,
`huxtable` 29, and `DT` 43. In contrast, `tinytable` imports no other
`R` package by default.

There `tinytable` is also “small” along several other dimensions:

-   The user interface is simple, streamlined, consistent, uncluttered.
-   The `tinytable` code base is very small and easy to maintain.
-   `tinytable` is a very thin wrapper around incredibly powerful
    frameworks: Bootstrap for HTML and `tabularray` for LaTeX/PDF. By
    staying “close to the metal”, `tinytable` allows users to create
    endlessly customizable tables:.
-   `tinytable` imports no 3rd party `R` package.
-   This package is free. Tiny tables for a tiny price!

## Installation

You can install the development version of tinytable from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("vincentarelbundock/tinytable")
```

## Get started

TODO
