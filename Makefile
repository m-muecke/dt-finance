README.md: README.qmd
	@quarto render $<

render: README.md

preview: README.qmd
	@quarto preview $<

.PHONY: render preview
