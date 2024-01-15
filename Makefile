.PHONY: help website

BOOK_DIR := book

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

check:  ## check package
	Rscript -e "devtools::document();devtools::check()"

website: ## render vignettes and website
	Rscript -e "devtools::document();devtools::install()"
	Rscript -e "altdoc::render_docs(verbose = TRUE)"
	rm -rf _quarto
	rm -rf ~/Downloads/tinytable_website && cp -r docs ~/Downloads/tinytable_website
