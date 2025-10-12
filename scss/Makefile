# Tabulator Reactable Theme Makefile
# Compiles SCSS to CSS with options for development and production

# Variables
SCSS_FILE = tabulator_tinytable.scss
CSS_FILE = tabulator_tinytable.css
CSS_MIN_FILE = tabulator_tinytable.min.css
EXAMPLE_FILE = example.html

# SASS compiler options
SASS_DEV_FLAGS = --style=expanded --source-map
SASS_PROD_FLAGS = --style=compressed --no-source-map

# Default target
.PHONY: all
all: dev

# Development build (expanded CSS with source maps)
.PHONY: dev
dev: $(CSS_FILE)

$(CSS_FILE): $(SCSS_FILE) tabulator.scss
	@echo "Compiling SCSS for development..."
	sass $(SASS_DEV_FLAGS) $(SCSS_FILE) $(CSS_FILE)
	@echo "Development CSS compiled successfully!"

# Production build (minified CSS without source maps)
.PHONY: prod
prod: $(CSS_MIN_FILE)

$(CSS_MIN_FILE): $(SCSS_FILE) tabulator.scss
	@echo "Compiling SCSS for production..."
	sass $(SASS_PROD_FLAGS) $(SCSS_FILE) $(CSS_MIN_FILE)
	@echo "Production CSS compiled and minified successfully!"

# Build both development and production versions
.PHONY: build
build: dev prod

# Watch for changes and recompile automatically
.PHONY: watch
watch:
	@echo "Watching for SCSS changes..."
	sass --watch $(SASS_DEV_FLAGS) $(SCSS_FILE):$(CSS_FILE)

# Clean compiled files
.PHONY: clean
clean:
	@echo "Cleaning compiled files..."
	rm -f $(CSS_FILE) $(CSS_FILE).map
	@echo "Clean complete!"

# Validate SCSS syntax
.PHONY: check
check:
	@echo "Checking SCSS syntax..."
	sass --check $(SCSS_FILE)
	@echo "SCSS syntax is valid!"

# Open example in default browser (macOS)
.PHONY: serve
serve: dev
	@echo "Opening example in browser..."
	open $(EXAMPLE_FILE)

# Install dependencies (if using npm for sass)
.PHONY: install
install:
	@echo "Installing SASS compiler..."
	@if command -v npm >/dev/null 2>&1; then \
		npm install -g sass; \
	elif command -v brew >/dev/null 2>&1; then \
		brew install sass/sass/sass; \
	else \
		echo "Please install SASS manually: https://sass-lang.com/install"; \
	fi

# Help target
.PHONY: help
help:
	@echo "Tabulator Reactable Theme Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  all/dev    - Compile SCSS for development (default)"
	@echo "  prod       - Compile and minify SCSS for production"
	@echo "  build      - Compile both development and production versions"
	@echo "  watch      - Watch for changes and recompile automatically"
	@echo "  check      - Validate SCSS syntax"
	@echo "  clean      - Remove compiled CSS files"
	@echo "  serve      - Compile and open example in browser"
	@echo "  install    - Install SASS compiler"
	@echo "  help       - Show this help message"
	@echo ""
	@echo "Files generated:"
	@echo "  $(CSS_FILE)           - Development CSS (expanded)"
	@echo "  $(CSS_MIN_FILE)       - Production CSS (minified)"
	@echo ""
	@echo "Example usage:"
	@echo "  make           # Compile for development"
	@echo "  make prod      # Compile for production"
	@echo "  make watch     # Watch and auto-compile"
	@echo "  make serve     # Compile and open in browser"
