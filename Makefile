README.md: README.Rmd
	@Rscript -e 'rmarkdown::render("$<", output_options = list(html_preview = FALSE))'

render: README.md

.PHONY: render preview
