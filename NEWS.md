# News

## Development

New:

- Support for `Typst` tables using `tablex`: 
  - https://typst.app/
  - https://github.com/PgBiel/typst-tablex
- `notes` argument in `tt()` can insert superscript markers inside cells to refer to notes at the bottom of the page.
  - `tt(x, notes = list("*" = list(i = 0:1, j = 2, text = "Hello world)))`

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
- Math in $$ is new recommendation.

## 0.0.1

Initial package release. Yay!
