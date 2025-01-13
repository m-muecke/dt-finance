README.md: README.Rmd
	@Rscript -e 'litedown::fuse("$<")'

render: README.md

preview: README.Rmd
	@Rscript -e 'litedown::roam(open = TRUE); while(TRUE) Sys.sleep(0.01)'

.PHONY: render preview
