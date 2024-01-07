#' 
#'
#' @section LaTeX `tabularray` options:
#'
#'
#' ## Table: `tinytable()`
#'
#'
#' Inner specifications:
#'
#'
#' | Key       | Description and Values                                    | Initial Value |
#' |-----------|-----------------------------------------------------------|---------------|
#' | `rulesep` | space between two hlines or vlines                        | `2pt`         |
#' | `stretch` | stretch ratio for struts added to cell text               | `1`           |
#' | `abovesep`| set vertical space above every row                        | `2pt`         |
#' | `belowsep`| set vertical space below every row                        | `2pt`         |
#' | `rowsep`  | set vertical space above and below every row              | `2pt`         |
#' | `leftsep` | set horizontal space to the left of every column          | `6pt`         |
#' | `rightsep`| set horizontal space to the right of every column         | `6pt`         |
#' | `colsep`  | set horizontal space to both sides of every column        | `6pt`         |
#' | `hspan`   | horizontal span algorithm: `default`, `even`, or `minimal`| `default`     |
#' | `vspan`   | vertical span algorithm: `default` or `even`              | `default`     |
#' | `baseline`| set the baseline of the table                             | `m`           |
#'
#' 
#' Outer specifications:
#'
#'
#' | Key       | Description and Values                          | Initial Value |
#' |-----------|-------------------------------------------------|---------------|
#' | `baseline`| set the baseline of the table                   | `m`           |
#' | `long`    | change the table to a long table                | None          |
#' | `tall`    | change the table to a tall table                | None          |
#' | `expand`  | you need this key to use verb commands          | None          |
#'
#'
#' ## Cells: `style_cell()`
#'
#'
#' | Key     | Description and Values                                                         | Initial Value |
#' |---------|---------------------------------------------------------------------------------|---------------|
#' | `halign`| horizontal alignment: `l` (left), `c` (center), `r` (right) or `j` (justify)   | `j`           |
#' | `valign`| vertical alignment: `t` (top), `m` (middle), `b` (bottom), `h` (head) or `f` (foot) | `t`       |
#' | `wd`    | width dimension                                                                | None          |
#' | `bg`    | background color name                                                          | None          |
#' | `fg`    | foreground color name                                                          | None          |
#' | `font`  | font commands                                                                  | None          |
#' | `mode`  | set cell mode: `math`, `imath`, `dmath` or `text`                              | None          |
#' | `cmd`   | execute command for the cell text                                              | None          |
#' | `preto` | prepend text to the cell                                                       | None          |
#' | `appto` | append text to the cell                                                        | None          |
#' | `r`     | number of rows the cell spans            | 1             |
#' | `c`     | number of columns the cell spans         | 1             |
#'
#'
#' ## Rows: `style_row()`
#'
#'
#' | Key        | Description and Values                                                              | Initial Value |
#' |------------|-------------------------------------------------------------------------------------|---------------|
#' | `halign`   | horizontal alignment: `l` (left), `c` (center), `r` (right) or `j` (justify)        | `j`           |
#' | `valign`   | vertical alignment: `t` (top), `m` (middle), `b` (bottom), `h` (head) or `f` (foot) | `t`       |
#' | `ht`       | height dimension                                                                    | None          |
#' | `bg`       | background color name                                                               | None          |
#' | `fg`       | foreground color name                                                               | None          |
#' | `font`     | font commands                                                                       | None          |
#' | `mode`     | set mode for row cells: `math`, `imath`, `dmath` or `text`                          | None          |
#' | `cmd`      | execute command for every cell text                                                 | None          |
#' | `abovesep` | set vertical space above the row                                                    | `2pt`         |
#' | `belowsep` | set vertical space below the row                                                    | `2pt`         |
#' | `rowsep`   | set vertical space above and below the row                                          | `2pt`         |
#' | `preto`    | prepend text to every cell (like `>` specifier in `rowspec`)                        | None          |
#' | `appto`    | append text to every cell (like `<` specifier in `rowspec`)                         | None          |
#'
#'
#' ## Columns: `style_column()`
#'
#'
#' | Key         | Description and Values                                                         | Initial Value |
#' |-------------|--------------------------------------------------------------------------------|---------------|
#' | `halign`    | horizontal alignment: `l` (left), `c` (center), `r` (right) or `j` (justify)  | `j`           |
#' | `valign`    | vertical alignment: `t` (top), `m` (middle), `b` (bottom), `h` (head) or `f` (foot) | `t`       |
#' | `wd`        | width dimension                                                                | None          |
#' | `co`        | coefficient for the extendable column (`X` column)                             | None          |
#' | `bg`        | background color name                                                          | None          |
#' | `fg`        | foreground color name                                                          | None          |
#' | `font`      | font commands                                                                  | None          |
#' | `mode`      | set mode for column cells: `math`, `imath`, `dmath` or `text`                  | None          |
#' | `cmd`       | execute command for every cell text                                            | None          |
#' | `leftsep`   | set horizontal space to the left of the column                                 | `6pt`         |
#' | `rightsep`  | set horizontal space to the right of the column                                | `6pt`         |
#' | `colsep`    | set horizontal space to both sides of the column                               | `6pt`         |
#' | `preto`     | prepend text to every cell (like `>` specifier in `colspec`)                   | None          |
#' | `appto`     | append text to every cell (like `<` specifier in `colspec`)                    | None          |
#' 
#'
#' ## hlines
#'
#' 
#' | Key         | Description and Values                                         | Initial Value |
#' |-------------|-----------------------------------------------------------------|---------------|
#' | `dash`      | dash style: `solid`, `dashed` or `dotted`                      | `solid`       |
#' | `text`      | replace hline with text (like `!` specifier in `rowspec`)      | None          |
#' | `wd`        | rule width dimension                                           | `0.4pt`       |
#' | `fg`        | rule color name                                                | None          |
#' | `leftpos`   | crossing or trimming position at the left side                 | `1`           |
#' | `rightpos`  | crossing or trimming position at the right side                | `1`           |
#' | `endpos`    | adjust leftpos/rightpos for only the leftmost/rightmost column | `false`       |
#' 
#' ## vlines
#'
#' 
#' | Key        | Description and Values                                        | Initial Value |
#' |------------|---------------------------------------------------------------|---------------|
#' | `dash`     | dash style: `solid`, `dashed` or `dotted`                     | `solid`       |
#' | `text`     | replace vline with text (like `!` specifier in `colspec`)     | None          |
#' | `wd`       | rule width dimension                                          | `0.4pt`       |
#' | `fg`       | rule color name                                               | None          |
#' | `abovepos` | crossing or trimming position at the above side               | `0`           |
#' | `belowpos` | crossing or trimming position at the below side               | `0`           |
#' 
