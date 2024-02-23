# News

## Development

`format_tt()` improvements:

* New `i` argument to format subsets of rows.
* New `fn` argument which accepts an arbitrary function to format table content.
* `num_fmt="significant_cell"` rounds significant digits on a cell-by-cell basis rather than for full columns (as is default in base R `format()`).
* Numeric formatting options can be set via global options, defined in the function signature.

Misc:

* Typst now supports images and inline plots with `plot_tt()`. Thanks to @aghaynes for contribution #155.
* Improved support for `tibble`. ANSI characters (ex: fancy `pillar` formatting) are stripped automatically or converted to HTML when the `fansi` package is installed. `fansi` is a dependency of `tibble`, so it should often be installed.
* New `tinytable_tt_digits` global option can set the default number of digits in the `tt()` function.

Breaking changes:

* In some cases, `format_tt()` could previously be applied sequentially to apply two formats to the same cell. Now, multiple calls to `format_tt()` can still be make chained with pipes, but they must apply to different cells with `i`, `j`, otherwise only the last change is respected. One exception is the `escape` argument which can be applied to pre-formatted cells.
* The "tinytable_tabularray_placement" global option is renamed to "tabularray_tt_placement".

## 0.0.5

* `format_tt()` escapes <> tags in Typst.
* Bug introduced in 0.0.4 prevented `group_tt(i)` in HTML.

## 0.0.4

New:

- `j` argument in `style_tt()` and `format_tt()` now accepts a string vector to match columns. Issue #122
- Line plots: `plot_tt(fun = "line")`
- `format_tt(j=NULL, escape=TRUE)` now escapes column headers in addition to all cells.
- `format_tt()` gains a `replace_na` argument to replace missing values.
- `style_tt()`: `rowspan` and `colspan` arguments are now supported in all formats except Typst. In markdown and Word, we get "pseudo-spans" with empty cells around the main cell, instead of true merged cells.
- `style_tt()`: `alignv` argument is now supported for LaTeX and HTML

Bugfix:

- Markdown group columns when labels are wider than columns. Thanks to @etiennebacher for report #127.
- Markdown group rows broke indexing when using `style_tt()`. Thanks to @strengejacke for report #133.


## 0.0.3

New:

- `Typst` tables are now supported using the `tablex` extension: 
  - https://typst.app/
  - https://github.com/PgBiel/typst-tablex
- `escape` argument in `format_tt()` escapes or substitutes special characters in LaTeX or HTML output to prevent compilation and rendering errors.
- `notes` argument in `tt()` can insert superscript markers inside cells to refer to notes at the bottom of the page.
  - `tt(x, notes = list("*" = list(i = 0:1, j = 2, text = "Hello world)))`
- `notes` agument in `tt()` now works wth Markdown and Word, but must be a single string.
- `group_tt()` can be called multiple times to create mult-row headers.


## 0.0.2

Improvements:

- Rules and cell borders: `line`, `line_width`, and `line_color` arguments.
- Enhanced knitr output detection.
- New themes.
- Caption argument support for Markdown tables.
- Defensive programming enhancements.
- plot_tt() regular expression column selection.
- Header/footer are no longer indented by group_tt(i).

Bug fixes:

- Inline plots visibility in RStudio viewer pane.
- Bug in group_tt(i) for markdown and docx output.
- Resolved style_tt resetting issue.
- Bug fix for column alignment in markdown affecting docx output.

Documentation:

- Improved vignette on the package website. 
- Various documentation updates.
- Math in $$ is the new recommendation.


## 0.0.1

Initial package release. Yay!
