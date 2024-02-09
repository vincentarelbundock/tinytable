# News

## Development

New:

- `j` argument in `style_tt()` and `format_tt()` now accepts a string vector to match columns. Issue #122
- Line plots: `plot_tt(fun = "line")`
- `format_tt(j=NULL, escape=TRUE)` now escapes column headers in addition to all cells.
- `style_tt()`: `rowspan` and `colspan` arguments are now supported for LaTeX and HTML tables.

Bugfix:

- Markdown group columns when labels are wider than columns. Thanks to @etiennebacher for report #127.

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
