.PHONY: help website

BOOK_DIR := book

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

document:  ## document
	Rscript -e "devtools::document()"

check:  document ## check
	Rscript -e "devtools::check()"

install: document  ## install
	Rscript -e "devtools::install(dependencies = FALSE)"

installdep: document  ## install with Suggests
	Rscript -e "devtools::install(dependencies = TRUE)"

test: install ## test
	Rscript -e "library(tinytable);tinytest::run_test_dir()"

typst: ## compile typst example
	quarto render sandbox/typst.qmd

website: install ## render vignettes and website
	rm -rf _quarto
	rm -rf docs
	quarto render sandbox
	Rscript -e "altdoc::render_docs(verbose = TRUE, freeze = TRUE, autolink = TRUE)"
