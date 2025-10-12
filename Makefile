.PHONY: help website tabulator-dev tabulator-prod tabulator-build tabulator-watch tabulator-clean tabulator-check tabulator-install tabulator-serve

TABULATOR_SCSS_FILE := inst/tabulator_tinytable.scss
TABULATOR_SCSS_DEP := inst/tabulator.scss
TABULATOR_CSS_FILE := inst/tabulator_tinytable.css
TABULATOR_CSS_MIN_FILE := inst/tabulator_tinytable.min.css
TABULATOR_EXAMPLE_FILE := inst/example.html

SASS_DEV_FLAGS := --style=expanded --source-map
SASS_PROD_FLAGS := --style=compressed --no-source-map

BOOK_DIR := book

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

document:  readme ## document
	Rscript -e "devtools::document()"

readme:  README.qmd ## readme
	quarto render README.qmd -t gfm

check:  document ## check
	Rscript -e "devtools::check()"

install: document  ## install
	Rscript -e "devtools::install(dependencies = FALSE)"

installdep: document  ## install with Suggests
	Rscript -e "devtools::install(dependencies = TRUE)"

test: install ## test
	Rscript -e "library(tinytable);tinytest::run_test_dir()"

website: ## render vignettes and website
	rm -rf _quarto
	rm -rf docs
	Rscript -e "altdoc::render_docs(verbose = TRUE, freeze = TRUE)"
	cp -r man/figures docs/man/

tabulator-dev: ## Compile Tabulator SCSS for development (expanded with source maps)
	@echo "Compiling Tabulator SCSS for development..."
	sass $(SASS_DEV_FLAGS) $(TABULATOR_SCSS_FILE) $(TABULATOR_CSS_FILE)
	@echo "Development CSS compiled successfully!"

tabulator-prod: ## Compile Tabulator SCSS for production (minified)
	@echo "Compiling Tabulator SCSS for production..."
	sass $(SASS_PROD_FLAGS) $(TABULATOR_SCSS_FILE) $(TABULATOR_CSS_MIN_FILE)
	@echo "Production CSS compiled and minified successfully!"

tabulator-build: tabulator-dev tabulator-prod ## Build both development and production CSS

tabulator-watch: ## Watch Tabulator SCSS and recompile on changes
	@echo "Watching Tabulator SCSS for changes..."
	sass --watch $(SASS_DEV_FLAGS) $(TABULATOR_SCSS_FILE):$(TABULATOR_CSS_FILE)

tabulator-clean: ## Remove compiled Tabulator CSS artifacts
	@echo "Cleaning compiled Tabulator CSS files..."
	rm -f $(TABULATOR_CSS_FILE) $(TABULATOR_CSS_FILE).map $(TABULATOR_CSS_MIN_FILE)
	@echo "Clean complete!"

tabulator-check: ## Validate Tabulator SCSS syntax
	@echo "Checking Tabulator SCSS syntax..."
	sass --check $(TABULATOR_SCSS_FILE)
	@echo "SCSS syntax is valid!"

tabulator-install: ## Install Sass compiler via npm or Homebrew
	@echo "Installing SASS compiler..."
	@if command -v npm >/dev/null 2>&1; then \
		npm install -g sass; \
	elif command -v brew >/dev/null 2>&1; then \
		brew install sass/sass/sass; \
	else \
		echo "Please install SASS manually: https://sass-lang.com/install"; \
	fi

tabulator-serve: tabulator-dev ## Compile Tabulator SCSS and open example in browser (macOS)
	@echo "Opening Tabulator example in browser..."
	@if [ -f "$(TABULATOR_EXAMPLE_FILE)" ]; then \
		open $(TABULATOR_EXAMPLE_FILE); \
	else \
		echo "Example file $(TABULATOR_EXAMPLE_FILE) not found."; \
	fi
