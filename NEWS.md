# News

## Development

`tt()`:

* `width` argument now accepts a vector of numeric values to control the width of each column, as a proportion of the total linewidth.

`format_tt()`:

* New `quarto` argument enables Quarto data processing for the whole table and marks specific cells as containing Quarto content. This is especially useful to include @Citation1981 in a table. Thanks to @andrewheiss for issue #215 and @giabaio for further discussion and debugging.
* New `replace` argument which accepts a single logical, a single string, or a named list to specify multiple replacements.
* `replace=TRUE` by default replaces `NA` by an empty string. `FALSE` prints "NA" as string.
* `replace_na` is deprecated in favor of `replace`. Backward compatibility is maintained and a warning is issued.
* All arguments can now be set using global options.

`theme_tt()`:

* "void" is now supported for Typst tables. Thanks to @marcboschmatas for PR #223.

`style_tt()`:

* No longer keep many versions of the same data frame, which could increase memory use. Thanks to @MarcoPortmann for the report.

`save_tt()`:

* Do not change working directory when saving to file raises an error. Thanks to @etiennebacher for report #225.

Typst:

* Better compatibility with Quarto captions. Recommended strategy is to *not* use the `caption` argument, and rather to define *both* the `label` and `tbl-cap` chunk options in Quarto. This is a breaking change, as Typst tables are no longer enclosed in a `#figure` environment in Quarto documents when both `tbl-cap` and `label` chunk options are defined.

Misc:

* Table objects can be modified and printed several times ---with styling--- in a single HTML document like a ReavealJS slideshow. Thanks to @kazuyanagimoto for report #759.
* Global option to enable Quarto data processing: `options(tinytable_quarto_disable_processing = FALSE)`. Thanks to @andrewheiss for issue #215.

Bug fixes:

* Data frames without column headers could be displayed as "c(1,2,3,3)". Bug in `format_tt()`. Issue #230.
* `save_tt()` can now save to PDF when the table includes a caption. Thanks to @etiennebacher for report #224.
* `group_tt(i)` inserted an extra latex column, which made horizontal lines stop to early. Thanks to @andrewheiss for report #235.


## 0.2.1

* RStudio displays table in HTML viewer by default when the `rstudioapi` package is available.
* `colnames` and `colnames<-` are now exported functions.
* `tt()` supports data.frame-like objects which also inherit from other classes, ex: `marginaleffects::slopes()`
* Bug: `options(tinytable_tt_print)` is respected in `print()` without argument.


## 0.2.0

New features:

* `rbind()` and `rbind2()` can be used to stack `tinytable` objects. `rbind2()` is more flexible than `rbind()`. See `?tinytable::rbind2`
* New output format in `print()`: "dataframe"
* Rename table headers: `colnames(tab) <- c("a", "b", "c")` 
* `theme_tt("resize")` gets a `direction` argument with "up", "down", "both" options. Thanks to @MarcoPortmann for feature request #207

Minor:

* Informative error message when no default browser is selected via global options.
* Fix CRAN errors on Mac old releases.

Bugs:

* `theme_tt()` resize issue with `talltblr` environment and notes in LaTeX. Thanks to @MarcoPortmann for reporting issue #206


## 0.1.0

New function `theme_tt()`:

* Function to apply collections of transformations to a `tinytable`.
* Visual themes:
  - grid, void, striped, bootstrap, default 
* `resize`: Insert a LaTeX table in a `resizebox` environment to ensure a table fits the page, or to scale it to a fraction of `\linewidth`
* `placement`: Determine where a LaTeX table float is positioned. Ex: `[H]`, `[htbp]`
* `multipage`: Split long LaTeX tables across multiple pages with (optional) repeated headers/footers. Uses the `longtblr` environment from `tabularray`.

`format_tt()`:

* New `i` argument to format subsets of rows.
* New `fn` argument which accepts an arbitrary function to format table content.
* `num_fmt="significant_cell"` rounds significant digits on a cell-by-cell basis rather than for full columns (as is default in base R `format()`).
* Numeric formatting options can be set via global options, defined in the function signature.
* `num_mark_big` and `num_mark_dec` require an explicit `digits`. We now raise an informative error.
* `escape = TRUE` now escapes captions, notes, and spanning cells created by `group_tt()` when `i` and `j` are both `NULL`. To avoid escaping group labels, users can specify `i` and/or `j` explicitly.

Typst format:

* Support for row headers with `group_tt(i)`
* Supports images and inline plots with `plot_tt()`. Thanks to @aghaynes for contribution #155.
* "kind: tinytable" parameter is now added to all figures enclosing a `tinytable`. This allows users to apply targeted show rules. For example, in a table of contents: `outline(target: figure.where(kind: "tinytable"))`

Misc:

* `style_tt()` gains a `finalize` argument. This accepts functions to be applied to the table object at the very end of the building process, to programmatically change its content. For example, this can be used with regular expressions to modify the text version of the table hosted in `tab@table_string`, or the function could programmatically modify the caption in `tab@caption`.
* `style_tt()`: LaTeX format supports decimal alignement with `align="d"`. The width of columns is determined by the maximum number of digits to the left and to the right in all cells specified by `i`, `j`.
* Support RevealJS slides in Quarto documents.
* Improved support for `tibble`. ANSI characters (ex: fancy `pillar` formatting) are stripped automatically or converted to HTML when the `fansi` package is installed. `fansi` is a dependency of `tibble`, so it should often be installed.
* New `tinytable_tt_digits` global option can set the default number of digits in the `tt()` function.
* Refactor: `tinytable` objects are now S4 class objects, with slots to hold data about the content and structure.
* `as.character()` now works on `tinytable` objects, returning a string in the output format specified by the `@output` slot of the `tinytable` object (markdown by default).
* LaTeX code in captions no longer requires double escaping, allowing: `tt(x, caption = "Blah blah \\label{tab:blah})`

Breaking changes:

* In some cases, `format_tt()` could be use sequentially to apply two formats to the same cell. Now, multiple calls to `format_tt()` can still be make chained with pipes, but they must apply to different cells with `i`, `j`, otherwise only the last change is respected. One exception is the `escape` argument which can be applied to pre-formatted cells.
* `tinytable` objects no longer have a `meta_tinytable` attribute. Use S4 slots instead.
* `placement` argument in `tt()` is removed in favor of `theme_tt("placement")`.

Bugs:

* `format_tt()` did not work on factor vector.


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
