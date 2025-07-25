# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## IMPORTANT

never call `library(tinytable)`. Always use `pkgload::load_all()` instead to use the latest code.

## Commands

### Development Commands
- `make test` - Run tests using tinytest framework
- `make check` - Run R CMD check with devtools
- `make document` - Generate documentation with devtools
- `make install` - Install package without dependencies
- `make installdep` - Install package with suggested dependencies
- `make readme` - Render README.qmd to README.md using Quarto
- `make website` - Build documentation website using altdoc

### Testing
- Test framework: `tinytest`
- Tests are located in `inst/tinytest/`
- Run single test: `Rscript -e "tinytest::run_test_file('inst/tinytest/test-[name].R')"`
- Test snapshots stored in `inst/tinytest/_tinysnapshot/`

### Manual Commands
If Makefile is not available:
- Document: `Rscript -e "devtools::document()"`
- Test: `Rscript -e "library(tinytable); tinytest::run_test_dir()"`
- Check: `Rscript -e "devtools::check()"`

## Architecture

### Core Components
- **Main Table Class**: S4 class `tinytable` defined in `R/class.R` with slots for body, data, styling, grouping
- **Core Function**: `tt()` in `R/tt.R` - main entry point for creating tables
- **Styling System**: `style_tt()` in `R/style_tt.R` - handles fonts, colors, alignment, borders
- **Formatting**: `format_tt()` - number/string formatting, escaping
- **Grouping**: `group_tt()` - row/column group labels and headers
- **Themes**: `theme_tt()` - pre-built styling collections

### Output Formats
Each format has dedicated modules:
- **HTML**: Bootstrap-based styling (`R/*_bootstrap.R`, `inst/templates/bootstrap.html`)
- **LaTeX**: tabularray-based (`R/*_tabularray.R`, `inst/templates/tabularray.tex`)
- **Typst**: Native Typst support (`R/*_typst.R`, `inst/templates/typst.typ`)
- **Grid/Markdown**: Text-based formats (`R/*_grid.R`)

### Design Philosophy
1. **Data-Style Separation**: Content kept separate from styling for clean, readable output
2. **Format-Specific Backends**: Each output format uses specialized, well-established frameworks
3. **Zero Dependencies**: Core package imports only base R `methods`
4. **S4 Object System**: Formal class definitions with slots for table components

### Key Files
- `R/tt.R` - Main table creation function
- `R/class.R` - S4 class definitions and slot management
- `R/style_tt.R` - Core styling interface
- `R/finalize_*.R` - Format-specific rendering
- `inst/templates/` - Output format templates
