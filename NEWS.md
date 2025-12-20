# News

## 0.15.2

New features:

* `theme_html(css_rule)` now accepts shortcut strings "tinytable" (default) or "dark" for built-in themes
* `theme_html(css_rule)` can now load CSS from local file paths or external URLs (http/https ending in .css)
* New dark theme stylesheet available via `theme_html(css_rule = "dark")` optimized for dark background websites
* Added CSS variables (`--tt-text-color`, `--tt-line-color`, `--tt-border-color`, etc.) for easier customization
* CSS variables are now used consistently for HTML output with tinytable engine, even with custom stylesheets

Bugs:

* Tabulator HTML output now preserves special characters in column names like question marks (Issue #611, thanks to @etiennebacher).
* Document `rbind2()` limitations around `format_tt()`/`style_tt()` and string coercion when stacking tables (Issue #612, thanks to @alexploner).
* `theme_latex(environment = "tabular")` now preserves captions (Issue #613, thanks to @brueckmann).
* LaTeX tables again respect automatic wrapping when spanning horizontal cells by propagating `tt(width = ...)` across merged columns (Issue #614, thanks to @bastienchassagnol).
* `theme_html(css_rule)` is respected with `portable=TRUE`
* `modelsummary` Issue 931: LaTeX preamble not automatically added. Thanks to @resulumit

## 0.15.1

Bugs:

* `style_tt()` with a logical matrix containing all `FALSE` values no longer throws an error (Issue #609, thanks to @EinMaulwurf).
* Fixed spurious Windows portable HTML warning when using `height` parameter with non-HTML output formats (Issue #608).

Misc:

* Skip a few tests on CRAN.
* `print(tab, "html")` always base 64 encodes images. This is useful because it makes files more portable when viewed in IDEs, and because it simplifies path handling in temporary directories across operating systems.

## 0.15.0

Breaking change:

* Order of operations changed. `group_tt()` are now applied before `style_tt()` and `format_tt()`

New features:

* `style_tt()` now supports comprehensive styling for Tabulator tables including bold, italic, color, background, fontsize, and more. Cell-specific styles persist across sorting and pagination operations.
* `theme_html(tabulator_search = "column")` adds header filters to each column in Tabulator tables, allowing per-column searching.
* `theme_striped()` now supports Tabulator tables with alternating row background colors.
* `plot_tt()` now supports Tabulator tables with JavaScript-based rendering for built-in plot types (`bar`, `barpct`, `histogram`, `line`, `density`).
* `plot_tt()` now supports images in Tabulator tables via the `images` parameter.
* `theme_html(tabulator_css_rule = ...)` now appends CSS rules instead of overwriting them, allowing multiple calls to combine styles.
* `plot_tt()` bar plots now accept `color` as a length-2 vector (`c(bar_color, background_color)`) to show progress bars with backgrounds in static formats (PNG, PDF, LaTeX). Use `xlim` to control the range. Note: Tabulator format uses only the first color.
* `theme_html(script = ...)` allows users to inject custom JavaScript code or script tags into HTML output. Useful for loading external JavaScript libraries like Plotly, D3, etc.

Changes:

* `plot_tt(fun = "barpct")` is deprecated but still supported for backward compatibility. Use `fun = "bar"` with `xlim = c(0, 1)` and a two-color vector instead.

Internal changes:

* Refactored `style_tt()` logical parameter handling: defaults changed from `FALSE` to `NULL`, with `NULL` values stored as `NA` in the style dataframe. Simplified `expand_style()` to use consistent "last non-NA wins" logic for all style properties including logical ones.

Bugs:

* Issue in Typst when calling `style_tt()` with both a `rowspan` and a `line`. Thanks to @eringrand for reporting Issue #592.
* `group_tt(j = "_")` (and other single-character delimiters) now errors when column names contain differing numbers of delimiters, preventing malformed spanning headers.

## 0.14.0

Breaking changes:

* `"gfm"` is no longer a valid `output` format. Use `output = "markdown"` with `theme_markdown(style = "gfm")` instead.
* `theme_empty()` drops every change made to this point, so the order of operations matter.
* `"html_portable"` is no longer a valid `output` format in `save_tt()` or `print()`. Use `theme_html(portable = TRUE)` instead.
* `plot_tt()` deprecates the `asp` argument. Use `width_plot` and `height_plot` instead.
* `theme_void()` becomes `theme_empty()` to avoid conflict with `ggplot2::theme_void()`.
* The experimental function `strip_tt()` was added very recently but is now removed. Use `theme_empty()` instead.

Deprecated global options:

* `tinytable_html_engine` 
  - Alternative: `theme_html(tab, engine = "bootstrap")`
* `tinytable_html_portable`
  - Alternative: `theme_html(tab, portable = TRUE)`
* `tinytable_latex_preamble` 
  - Alternative: `theme_latex(tab, preamble = TRUE)`
* `tinytable_pdf_engine` 
  - Alternative: `theme_latex(tab, engine = "pdflatex")`
* `tinytable_quarto_figure`
  - No alternative.

New functions:

* `format_vector()` is similar to `format_tt()` but accepts and returns vectors.
* `style_vector()` is similar to `style_tt()` but accepts and returns vectors.
* `plot_vector()` is similar to `plot_tt()` but it returns a character vector of links to plots or images, with appropriate markup and styling, like `href` for HTML output or `\includegraphics{}` for LaTeX.

New arguments:

* `format_tt(output = )` to apply formatting conditionally based on output format.
* `style_tt(smallcap = TRUE)` to style text in small capitals. In HTML, LaTeX, and Typst, it uses proper small caps formatting. In Markdown, it converts text to uppercase.
* `tt(x, colnames = "label")` renames column names using the `attr(df$x, "label")` attribute, when available, falling back to column names otherwise.
* `format_tt(linebreak = "<br>")` can substitute a user-specified string to an appropriate character sequence to generate line breaks in the `output` of the `tinytable` (ex: `\\` for LaTeX, `<br>` for HTML, etc.)
* `theme_markdown(ansi=TRUE)` enables support for `style_tt()` colors, backgrounds, and text styles in Markdown output. 
* `theme_markdown(vline=FALSE, hline=FALSE)` suppresses lines.
* `plot_tt(height_plot, width_plot)` control the size (in pixels) of the plotting device canvas. This allows users to control the resolution of the original plot, as well as the relative size of elements.
* `plot_tt(fun = "barpct", color = "red", background = "blue")` draws percentage bars with two colors for p and 1-p.
* `plot_tt(sprintf = "...")` allows custom formatting of generated cell content with sprintf patterns.

New miscellaneous features:

* New Gallery vignette with advanced examples: https://vincentarelbundock.github.io/tinytable/vignettes/gallery.html
* Non-standard evaluation is supported for `i` and `j` arguments in `style_tt()`, `format_tt()`, and `plot_tt()`.
* `tt()` is now a generic function, allowing special table-drawing methods for specific classes of objects. `tinytable` provides methods for `data.frame`, `data.table`, and `tbl_df`. See the "Custom" vignette for examples.
* `subset(x, select = c(x, y))` can now be used to select columns.
* A global option can be used to choose the temporary directofy where HTML files are saved for preview: `options(tinytable_tempdir = "/home/username/tempdir")`
* Tabulator interactive tables support images with `plot_tt()`.
* `plot_tt()` can add images to replace column names. Issue #566.
* Improved path handling in `plot_tt()`

Bugs:

* `tabulator` output works with hyphens in columns names
* Selective formatting with `format_tt(i = "colnames")` is no longer applied twice. Issue #551.
* `color` ignored in matrix indexing.
* Fixed bug in non-standard evaluation with the `i` condition matches nothing. Thanks to @RJDan for report #564.
* `group_tt(j = )` respects column widths in bootstrap HTML output.
* Better path handling in `plot_tt()`
* `save_tt("typst")` no longer aborts if there is `typst` directory. Thanks to @sTeADone for report #580.


## 0.13.0

* New aliases to facilitate completion in IDEs: `tt_style()`, `tt_format()`, `tt_plot()`, `tt_group()`, `tt_save()`. Thanks to @rpruim for the suggestion in Issue #540.
* Tabularray: use `font=` instead of `cmd=` for italic, bold, and monospace. This allows using more than one style in a single cell. Thanks to @lvjr for the recommendation in Issue #524.

Bugs:

* `theme_latex()`: support removal of `environment_table` when `environment=NULL`. Thanks to @wklimowicz for report #535.

## 0.12.0

Deprecated or replaced features:

* `theme_tt()` function has been deprecated. Use format-specific or style-specific theme functions instead:
  - Format-specific: `theme_html()`, `theme_latex()`, `theme_typst()`
  - Style-specific: `theme_grid()`, `theme_revealjs()`, `theme_rotate()`, `theme_striped()`, `theme_void()`
* `output` argument from `style_tt()`.
* `tabularray_inner` and `tabularray_outer` arguments from `style_tt()`. Use `theme_latex()` instead.
* `bootstrap_class`, `bootstrap_css`, and `bootstrap_css_rule` arguments from `style_tt()`. Use `theme_html()` instead.
* `options(tinytable_latex_float_placement)` has been deprecated. Use `options(tinytable_latex_placement)` instead.

New:

* The Tabulator JS framework can now be used to create interactive HTML tables with filtering, pagination, search, etc. `tt(x) |> theme_tt("tabulator") |> print("tabulator")`
* `theme_*()` functions are now systematically documented and much more robust, instead of being routed via `theme_tt()`.
* `group_tt(j = "_")` allows multiple delimiters in column names to easily create multiple level headers. Thanks to @nmercadeb for feature request #518.

Bug fixes:

* `group_tt(i = x)` works when `x` is a factor vector.
* Typst: fix backslash escaping in `format_tt(escape=TRUE)`

## 0.11.0

Breaking changes:

* `theme_spacing()` function has been removed. Use the new `height` argument in `tt()` instead for row height control.
* Indexing for `format_tt()` is now consistent with `style_tt()` and `plot_tt()`. It refers to rows and columns in the final table, after insertions by `group_tt()`.
* Argument `indent` is removed form the `group_tt()` function. Use `style_tt(x, "~groupi", indent = 1)` instead.
* `print("dataframe")` is now consistent with other formats and only prints the result. Use `save_tt("dataframe")` to obtain a data frame.

New features:

* Shortcuts available in `i` for `style_tt()` and `format_tt()`:
  - `"group"`: row group rows (complement of "groupi")
  - `"~groupi"`: complement of "groupi"
  - `"groupj"`: header rows
  - `"colnames"`: column names
  - `"caption"`: caption
  - `"notes"`: notes
* `tt(height=2)` controls row height in em units. Works HTML, LaTeX, and Typst.
* `format_tt()` argument `i` accepts character strings to format specific table components: "colnames", "caption", "notes", "groupi" (row group labels), "groupj" (column group labels).
* `group_tt(i = ..., j = ...)` now supports matrix insertion: when `i` is an integer vector and `j` is a character matrix, rows from the matrix are inserted at the specified positions. Single-column matrices are automatically reshaped when the number of elements is a multiple of the table's column count.
* `group_tt(i = ...)` can now be called multiple times to insert several rows one after the other.
* `style_tt(colspan)` is now supported for Markdown and Typst outputs.
* `style_tt(i = "~groupi")` styles all non-group rows (complement of "groupi").
* `options(tinytable_color_name_normalization=TRUE)` (default): automatic color name processing (default: TRUE). When enabled, R color names recognized by `col2rgb()` are converted to hex format for consistent rendering across HTML, LaTeX, and Typst formats. If R color conversion fails, LaTeX color names are used as fallback. Colors explicitly supplied as hex values with "#" prefix are passed through unchanged. Set to FALSE to disable processing and pass color names unchanged.

Typst: Major refactor improves several things and brings Typst very close to feature parity with other formats.

* Typst code is not **much** more efficient and concise. Tables over 500 pages long compile in mere seconds. Thanks to @brynhum for the report and tips on implementation #475.
* `rowspan` and `colspan` arguments are now supported, with informative error messages when they are not used correctly.
* Support for `alignv` argument in `style_tt()`. This allows vertical alignment of cells in Typst tables.
* `plot_tt()` supports height.
* Support `rowsep` in `theme_tt("spacing")`.

Misc:

* Issue #392: reinstate informative error when `format_tt()` uses `num_mark_big` without `digits` argument.
* Improvements to `theme_tt("revealjs")`.
* Improvements to markdown output with long cell content.

Bugs:

* Fixed matrix row duplication in `group_tt()`: single matrix rows are now properly duplicated when inserted at multiple positions using syntax like `group_tt(i = c(2, 5), j = matrix)`.
* Fixed bootstrap header issues in HTML tables with grouped data.
* Fixed duplicate header problems in LaTeX/Tabularray output.
* Fixed long row handling in markdown output format.
* Fix `theme_bootstrap()` for LaTeX and Typst. Issue #479.
* `theme_tt("multipage")` correctly converts LaTeX environment to `longtblr`. Thanks to @ujtwr for report #493.

Internal changes:

* Major refactor of `group_tt()` architecture: removed legacy lazy evaluation system (`@lazy_group_i`, `@lazy_group_j`) in favor of direct data structure manipulation with `@group_data_i` and `@group_data_j` slots.
* Simplified `format_tt()` function implementation for better performance and maintainability.
* Enhanced Typst backend with improved header handling and code efficiency.
* Improved grid backend with colspan support for text output.
* Comprehensive expansion of snapshot tests for matrix insertion functionality.
* Streamlined class architecture by removing unused legacy code paths.

## 0.10.0

Bugs:

* Major refactor of `rowspan` for HTML files. Thanks to @J-Moravec and @ASKurz for reports #355 and #457.
* `theme_tt(x, "multipage")` failed to insert `rowhead` in LaTeX. Thanks to @sTeADone for report #460.
* Error when `x` is a data.table without columns.
* Adjust Markdown table width when `group_tt(i = )` labels are long. Thanks to @JohannesNE for report #469.

Misc:

* `group_tt()` argument `j` accepts a single string as delimiter (ex: `"_"`) when column names of the data frame include group names. Thanks to @sda030 for the feature request, a first implementation, and testing.
* `style_tt(i = "groupi")` styles group rows.
* `theme_revealjs()` now supports dark theme with `css = "dark"` argument.
* Many improvements to the documentation and vignettes
* New vignette with table of contents for CRAN
* `tt()` gets a `colnames` argument, instead of having to delete names manually.

## 0.9.0

New:

* `strip_tt()` function removes elements of an existing `tinytable` object.

Bugs:

* Typst footnotes with cell coordinates. Thanks to @sverrirarnors for report #456.

Docs improvement:

* Quarto issue with `-` as list. Thanks to Chris Goodman.

## 0.8.0

New:

* Support for `litedown` notebooks with "latex", "html", or "markdown" output.

Bugs:

* `format_tt()` works on tibbles. Thanks to @jon-mellon for report #426.
* Respects the "Chunk Output in Console" option in RStudio. Thanks to @richardohrvall for issue #851 on the `modelsummary` repository.
* Function themes broke HTML tables. Thanks to @kazuyanagimoto for report #439.
* `tabularray` LaTeX update breaks `theme_tt("rotating")`. Thanks to @kylebutts for report #444.

Tests:

* Using the latest `tinysnapshot` means snapshot tests are no longer sensitive to random seeds.

## 0.7.0

Breaking change:

* `format_tt()` is now stricter, applying no formatting at all by default. Users must specify an argument explicitly or set a global option to change the table.
* S4 slots renamed for internal consistency and clarity.

Bugs:

* `save_tt("file.pdf")` works with colors. Thanks to @olivedv for the report and solution #395.
* `group_tt(i=vec)`: `vec` can be a factor vector
* `style_tt(align="d")` with empty strings (`modelsummary::datasummary_balance()` test)
* `style_tt(line_color)` accepts Hex codes. Thanks to @andrewheiss for report #415.
* `tt(rownames=TRUE)` should not add column names if they do not exist. Thanks to @Nowosad for report #414.
* `style_tt(tabularray_outer = ...`) correctly inserts argument in outer. Issue #419. Thanks to @wklimowicz

New:

* `style_tt("notes")` and `style_tt("caption")` can style footnotes and captions. Note: This will only style captions handled by the `caption` argument in `tt()`, and not captions created by Quarto.
* Using `table@group_index_i` is now documented.

Misc:

* Documentation improvements

## 0.6.1

* Bug fix: d-column LaTeX generated an error in some cases.

## 0.6.0

* Major refactor of the style internals. HTML, LaTeX, and Typst documents should be much more concise and efficient.
* `theme_tt("spacing")`: Change the row and column spacing to create more compact or airy tables. LaTeX and HTML only. Thanks to @statzhero for feature request #353.
* `style_tt()`: the `i` and `j` indices are now consistent in all formats. They refer to rows *after* the insertion of  row groups.
* `save_tt()` respects `options(tinytable_save_overwrite=TRUE)`
* LaTeX: Guard header rows when using `style_tt(align="d")`. Issue #367
* Inline display in Quarto and Rmarkdown notebooks. `options(tinytable_print_rstudio_notebook = "inline")` or `"viewer"`

Bugs:

* Typst notes returned an error since the last release. Thanks to @DominikVogel for report #357.
* Duplicate group labels are allowed in LaTeX with `group_tt()`. Thanks to @eeemda for report #362.


## 0.5.0

New:

* `output="html_portable"` returns a portable HTML file, where `plot_tt()` encodes and embeds the images directly in the HTML code, rather than link to external images. Thanks to @J-Moravec for implementing this nice feature!
* `format_tt()` gets a `math` argument to wrap cell content in $...$ math mode.
* `group_tt(i = vec)` accepts a vector of labels of length equal to the number of rows in the dataset.
* `tt()` gets an `escape` argument. Thanks to Cameron Patrick for the feature request.
* The `i` argument in `style_tt()` now accepts a logical matrix of same dimensions as `x`, to style specific cells, rather than all combinations of `i` and `j` vectors. Thanks to @dhicks for the feature request #329.
* `style_tt()` gets new `output` argument for conditional styling based on output format.
* `names()` method now supported for both returning column names and re-assingning them. Issue #332.

Typst:

* Table code is much more concise and efficient.
* Fix indexing bug for groups. Issue #323 and #343.
* `style_tt()` can override cell styling with successive calls, and the call order is respected.
* `options(tinytable_quarto_figure = FALSE)` wraps Typst tables in a `figure` environment in Quarto documents.

Bugs:

* Fixed bug in replacing `cmidrule`. Thanks to @jsr-p for code submission #349.

## 0.4.0

### Breaking change

HTML tables no longer insert MathJax scripts by default. This behavior could enter in conflict with other MathJax scripts loaded explicitly by the user or automatically by Quarto. Users can revert to the previous behavior by setting a global option:

`options(tinytable_html_mathjax = TRUE)`

Alternatively, users can insert appropriate scripts directly in their HTML document or as a Quarto literal chunk:

````
```{=html}
<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
<script>
MathJax = {
  tex: {
    inlineMath: [['$', '$'], ['\\(', '\\)']]
  },
  svg: {
    fontCache: 'global'
  }
};
</script>
```
````

* Option `tinytable_markdown_hlines` has been removed. To get a more minimal looking markdown table, use output "gfm" which is gfm compliant.

### General

* Global options are more consistent and better documented. Thanks to @kylebutts for PR #313.
* Support Viewer Pane in Positron IDE. Thank to @kylebutts for code contribution in PR #299.
* Improved documentation.
* `format_tt(markdown=TRUE)` escapes groups and notes when `i` and `j` are `NULL`.
* `plot_tt()`: The `height` argument is now respected in Markdown and Word documents.
* `group_tt()` allows 0 and duplicates in `i` argument for labels in the first row and consecutive labels.
* Headers are now styled and formatted when `i=NULL`.
* `colnames(x)<-NULL` works on a `tinytable` object.
* `format_tt(num_big_mark)` applies to integer columns.
* Use `getOption("viewer")` instead of `rstudioapi::viewer()` for positron support
* `glue::glue()` string is accepted by `format_tt()`. Thanks to @LukasWallrich for report #792 on the `modelsummary` repository.
* Support Github Flavored Markdown (`gfm`) output. Thanks to @kylebutts for contribution #315.
* `theme_tt("rotate")` to rotate tables in LaTeX or Typst.
* `save_tt("/path/to/file")` returns the file path invisibly. Thanks to @yjunechoe for issue #328.

### HTML

* Simplify JS functions in HTML documents. Avoid nesting full HTML documents inside Quarto output.
* Remove polyfill JS because of security issues.
* Avoid error in interactive use in Positron.

### LaTeX

* `theme_tt("tabular")` no longer uses `tabularray` or `booktabs`. Only relies on basic LaTeX packages.
* `theme_tt("tabular", style = "tabularray")` does the same as above, but keeps the `\begin{tblr}` environment.

### Typst

* `style_tt()` supports `align` for different rows and cells, rather than just whole columns.
* `style_tt()` supports `indent` argument.
* `group_tt()` supports `indent` argument.
* No more gutters when `group_tt(j)` and `style_tt(background)`
* `theme_tt(x, horizontal = "l")` can left, center, or right-align a table in the page.

### Global options

* `save_tt("pdf")`:
  - `options(tinytable_save_pdf_clean = TRUE)`
  - `options(tinytable_save_pdf_engine = "xelatex")`
* `options(tinytable_tt_rownames=TRUE)`: Print row names in the first column by calling. Thanks to @rsbivand for Issue #264.
* EXPERIMENTAL: `options(tinytable_html_mathjax = TRUE)`. Inserts MathJax scripts in the HTML file. This may cause conflicts if MathJax is otherwise loaded in the document.

### Bugs

* Footnotes were center-aligned in some Quarto chunks. Thanks to @andrewheiss for report #303.
* `replace` does not work in LaTeX with `format_tt(quarto=TRUE)`. Thanks to @cbgoodman for Issue #263.
* `style_tt(indent)` works for LaTeX
* Notes were hard-coded to 5 colspan. We now use the actual number of columns in the table. Thanks to @DominikVogel for report #788.
* Do not suppress labels when inserting notes. Thanks to @cportner for Issue #290.
* `format_tt()` on a table without column names. Thanks to @andrewheiss for report #306.
* \cmidrule[lr] requires [] in tabularray but () otherwise. Thanks to @andrewheiss for report #307.

## 0.3.0

Breaking change:

* The `width` argument is moved from `style_tt()` to `tt()`.

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
* `format_tt(escape=TRUE)` escapes square brackets.
* Tables are centered by default.

Misc:

* Support `beamer_presentations`, but see: https://github.com/vincentarelbundock/tinytable/issues/244
* Table objects can be modified and printed several times ---with styling--- in a single HTML document like a ReavealJS slideshow. Thanks to @kazuyanagimoto for report #759.
* Global option to enable Quarto data processing: `options(tinytable_quarto_disable_processing = FALSE)`. Thanks to @andrewheiss for issue #215.

Bug fixes:

* Data frames without column headers could be displayed as "c(1,2,3,3)". Bug in `format_tt()`. Issue #230.
* `save_tt()` can now save to PDF when the table includes a caption. Thanks to @etiennebacher for report #224.
* `group_tt(i)` inserted an extra latex column, which made horizontal lines stop to early. Thanks to @andrewheiss for report #235.
* Multiple unnamed footnotes allowed in LaTeX tabularray. Issue #242.


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
