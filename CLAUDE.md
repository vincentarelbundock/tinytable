# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## User instructions

Whenever running code from this package, always run `pkgload::load_all()` first to ensure the latest version is loaded. Never use `library(tinytable)`.

To run all tests: `pkgload::load_all(); tinytest::run_test_dir()`

Many operations inside `tinytable` are "lazy", and the S4 slots will not be filled until a table is "built". To build a table in a given format, call `print(x, "html")` or `build_tt(x, "html")` where `x` is a `tinytable` object. This will trigger the lazy evaluation and fill the S4 slots with the necessary data.

`x@style` is only filled on print, so use `print(x, "html")` and `cat()` to debug.

### Development workflow
- Always use `pkgload::load_all()` before testing any code changes
- Test changes with specific test files: `tinytest::run_test_file("inst/tinytest/test-html.R")`
- Use `make check` to run R CMD check before committing changes

## Project Overview

`tinytable` is an R package for creating tables in HTML, LaTeX, Markdown, Word, PDF, PNG, and Typst formats. Zero-dependency design (only `methods` package). User interface is built around a few core functions: `tt()`, `style_tt()`, `format_tt()`, `group_tt()`, `theme_tt()`, `save_tt()`.

## Commands

### Testing
- `make test` - Install package and run full test suite using tinytest
- `pkgload::load_all(); tinytest::run_test_file("inst/tinytest/test-html.R")` - Run a specific test file
- `pkgload::load_all(); tinytest::run_test_dir("inst/tinytest/", pattern = "test-group")` - Run tests matching pattern

### Development
- `make install` - Install package without dependencies
- `make document` - Generate R documentation using devtools
- `make check` - Run R CMD check (runs `document` first)
- `make website` - Render vignettes and rebuild website using altdoc

### Tabulator CSS (for HTML tabulator engine)
- `make tabulator-build` - Compile SCSS to both dev and minified CSS
- `make tabulator-watch` - Watch SCSS and recompile on changes

## Architecture

### Core Table Creation Flow
1. **`tt()`** (`R/tt.R`) - Creates `tinytable` S4 objects from data frames
2. **`build_tt()`** (`R/build_tt.R`) - Central processing pipeline: sanitizes output format → swaps S4 class → calculates body indices → applies lazy format/style/prepare/plot/finalize operations → dispatches to format backend
3. **Format-specific backends**: HTML (`html_*.R` with bootstrap/tabulator engines), LaTeX (`tabularray_*.R`), text/markdown (`grid_*.R`), Typst (`typst_*.R`)
4. **Finalization**: `finalize_*.R` functions render the final output string

### S4 Class (`R/aaa_class.R`)
Key slots on the `tinytable` class:
- **`@data`** - Original unmodified input data frame
- **`@data_body`** - Working data frame modified during processing
- **`@group_data_i`** / **`@group_data_j`** - Row/column group header data frames
- **`@index_body`** / **`@group_index_i`** - Position indices for row mapping
- **Lazy evaluation**: `@lazy_format`, `@lazy_style`, `@lazy_prepare`, `@lazy_plot`, `@lazy_finalize` - store operations applied in order during `build_tt()`
- **`@style`** / **`@style_other`** / **`@style_lines`** - Style data frames populated during build
- **`@output`** - Target format string (html, latex, typst, markdown, etc.)

### Key Design Patterns
- **Lazy evaluation**: `style_tt()`, `format_tt()`, `group_tt()`, `theme_tt()` append closures to `@lazy_*` slots. These are executed in sequence during `build_tt()`. This handles order dependencies.
- **Centralized processing**: All data transformations happen in `build_tt()` before backend dispatch. The `rbind_body_groupi()` function merges `@data_body` and `@group_data_i` using position indices.
- **Backend isolation**: Each format backend receives fully-processed data and only handles rendering.

### File Organization
- `R/` - 70+ source files organized by prefix:
  - `aaa_class.R` (S4 class), `build_tt.R` (pipeline), `tt.R` (entry point)
  - `html_*.R`, `tabularray_*.R`, `grid_*.R`, `typst_*.R`, `tabulator_*.R` - format backends
  - `style_*.R`, `group_*.R`, `finalize_*.R` - per-backend operations
  - `theme_*.R` - predefined themes
- `inst/tinytest/` - Tests using `tinytest` framework
- `inst/templates/` - Output format templates
- `vignettes/` - Package documentation

### Testing Patterns
- Tests use `tinytest` with `tinysnapshot` for visual regression testing
- All test files start with `source("helpers.R")` and `using("tinysnapshot")`
- `helpers.R` defines `expect_table()` for multi-format snapshot testing and `strip_random()` to remove random HTML IDs for deterministic comparisons
- Snapshot files live in `inst/tinytest/_tinysnapshot/`
- Update snapshots carefully — they capture exact output for each format

## Output Format Details
- **HTML**: Two engines — Bootstrap CSS (default) and Tabulator.js (interactive tables with sorting/filtering). Engine set via `tt(x, engine = "tabulator")`
- **LaTeX**: Uses `tabularray` package. Inner/outer specifications stored in `@tabularray_inner`/`@tabularray_outer`
- **Typst**: Modern typesetting system
- **Markdown**: GitHub Flavored Markdown via grid-based text rendering
- **Word**: Limited styling, via pandoc
- **PDF/PNG**: Via LaTeX compilation or web rendering
