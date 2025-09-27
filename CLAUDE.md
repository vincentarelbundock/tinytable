# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## User instructions

Whenever running code from this package, always run `pkgload::load_all()` first to ensure the latest version is loaded. Never use `library(tinytable)`.

To run all tests: `pkgload::load_all(); tinytest::run_test_dir()`

Many operations inside `tinytable` are "lazy", and the s4 slots will not be filled until a table is "built". To build a table in a given format, call `print(x, "html")` or `build_tt(x, "html")` where `x` is a `tinytable` object. This will trigger the lazy evaluation and fill the S4 slots with the necessary data.

x@style is only filled on print, so use `print(x, "html")` and `cat()` to debug.

### Development workflow
- Always use `pkgload::load_all()` before testing any code changes
- Test changes with specific test files: `tinytest::run_test_file("inst/tinytest/test-html.R")`
- Use `make check` to run R CMD check before committing changes

## Project Overview

`tinytable` is an R package for creating beautiful, customizable tables in multiple output formats (HTML, LaTeX, Markdown, Word, PDF, PNG, Typst). The package follows a minimal, zero-dependency design philosophy with a streamlined user interface built around a few core functions.

## Commands

### Testing
- `make test` - Install package and run full test suite using tinytest
- `Rscript -e "library(tinytable);tinytest::run_test_dir()"` - Run tests directly
- `tinytest::run_test_file("inst/tinytest/test-[format].R")` - Run specific test file (html, latex, markdown, etc.)
- `tinytest::run_test_dir("inst/tinytest/", pattern = "test-group")` - Run tests matching pattern

### Development
- `make install` - Install package without dependencies
- `make installdep` - Install with all suggested dependencies
- `make document` - Generate R documentation using devtools
- `make check` - Run R CMD check
- `make readme` - Render README.qmd to README.md using Quarto

### Documentation and Website
- `make website` - Render vignettes and rebuild entire website using altdoc

## Architecture

### Core Table Creation Flow
1. **`tt()`** - Main entry point that creates tinytable objects from data frames
2. **`build_tt()`** - Central processing function that handles matrix combination, lazy evaluation, formatting, and styling
3. **Format-specific backends**: HTML (`html_*` files with bootstrap/tabulator engines), LaTeX (`tabularray_*` files), text (`grid_*` files), Typst (`typst_*` files)
4. **Finalization**: Format-specific finalize functions render the final output

### S4 Class Architecture
The `tinytable` S4 class uses these key data slots:
- **`@data`** - Original unmodified input data frame
- **`@data_body`** - Working data frame that gets modified during processing
- **`@data_group_i`** - Row group header data frame
- **`@data_group_j`** - Column group header data frame
- **Index slots** - `@index_body`, `@index_group_i`, `@group_index_i` for positioning data
- **Lazy evaluation slots** - `@lazy_format`, `@lazy_style`, `@lazy_theme`, `@lazy_plot`, `@lazy_finalize` store operations to be applied later
- **Metadata slots** - `@nrow`, `@ncol`, `@nhead`, `@names`, `@output` track table dimensions and properties

### Matrix System for Data Management
The package uses a centralized matrix combination approach:
- Data transformations (grouping, formatting) happen in `build_tt()` before backend processing
- Each backend receives the final processed data via `@data_body` (the working data frame)
- Group insertions are handled by row/column index mapping (`@index_body`, `@index_group_i`)
- The `rbind_i_j()` function combines `@data_body` and `@data_group_i` using position indices
- This ensures all backends work with consistent, fully-processed data

### Key Design Patterns
- **S4 class system**: Tables are S4 objects with slots for data, styling, grouping, etc.
- **Method dispatch**: Different output formats handled through S4 method dispatch
- **Lazy evaluation**: Operations are stored and applied during `build_tt()` to handle order dependencies
- **Separation of data and style**: Content is kept separate from formatting/styling information
- **Centralized processing**: `build_tt()` is the single point where data transformations occur

### Main Functions
- `tt()` - Create table objects (R/tt.R)
- `style_tt()` - Apply styling (fonts, colors, alignment, etc.)
- `format_tt()` - Format numbers, dates, strings
- `group_tt()` - Add row/column group labels
- `theme_tt()` - Apply predefined styling themes
- `save_tt()` - Export to files

### File Organization
- `R/` - Main package code
  - `aaa_class.R` - S4 class definition and core methods
  - `build_tt.R` - Central processing pipeline
  - Backend implementations: `html_*.R` (HTML), `tabularray_*.R` (LaTeX), `grid_*.R` (text), `typst_*.R` (Typst), `tabulator_*.R` (HTML Tabulator engine)
  - `style_*.R` - Styling functions for each backend
  - `group_*.R` - Grouping functions for each backend
  - `finalize_*.R` - Final rendering for each backend
  - `theme_*.R` - Predefined styling themes
- `inst/tinytest/` - Test files using the tinytest framework
- `inst/templates/` - Output format templates
- `vignettes/` - Package documentation and tutorials

### Test Structure
Uses `tinytest` framework with snapshot testing via `tinysnapshot`. Test files are organized by functionality (`test-html.R`, `test-latex.R`, `test-style_tt.R`, etc.) with corresponding snapshot files in `_tinysnapshot/`.

## Output Format Support
- HTML: Two engines available - Bootstrap CSS framework (default) and Tabulator.js for interactive tables
- LaTeX: tabularray package
- Markdown: GitHub Flavored Markdown
- Typst: Modern typesetting system
- Word: Limited styling via pandoc
- PDF/PNG: Via LaTeX compilation or web rendering

## Development Notes
- Package uses only base R dependencies (methods package only)
- Suggested packages provide enhanced functionality but are not required
- Heavy use of S4 classes and method dispatch for clean architecture
- Each output format has its own complete styling pipeline
- Extensive test coverage with snapshot testing via `tinysnapshot` for visual outputs
- The `build_tt()` function is critical - all data transformations must happen there to maintain consistency across backends

## Important Development Patterns
- **Order matters in `build_tt()`**: Lazy evaluation ensures operations are applied in correct sequence
- **Index-based data management**: Row/column positions are tracked via index slots, not direct data manipulation
- **Backend isolation**: Each format backend (`html_*`, `tabularray_*`, `grid_*`, `typst_*`, `tabulator_*`) works with finalized data
- **Snapshot testing**: Visual regression testing captures exact output for each format - update snapshots carefully
