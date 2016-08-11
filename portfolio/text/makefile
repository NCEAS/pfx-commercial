TEXT = anderson-etal-rev-pfx
MS = ms
SOM = supp
REFS = refs

all: $(TEXT).pdf $(SOM).pdf

%.md: %.Rmd
	Rscript -e "knitr::knit('$<')"

%.tex: %.md
	pandoc -S --no-wrap --bibliography=$(REFS).bib --natbib -fmarkdown-all_symbols_escapable $< -o $@
	perl -p -i -e "s/Figure /Figure~/g" $@
	perl -p -i -e "s/Fig\. /Fig.~/g" $@
	perl -p -i -e "s/vs\. /vs.~/g" $@
	perl -p -i -e "s/Table /Table~/g" $@
	perl -p -i -e "s/Figs\. /Figs.~/g" $@

$(TEXT).pdf: $(MS).tex $(TEXT).tex $(REFS).bib
	latexmk $(TEXT)

$(SOM).pdf: $(SOM).tex $(REFS).bib
	latexmk $(SOM)

dropbox: $(TEXT).pdf rtf
	cp $(TEXT).pdf ~/Dropbox/Public/$(TEXT).pdf
	cp $(TEXT).rtf ~/Dropbox/Public/$(TEXT).rtf

rtf: $(TEXT).pdf
	latex2rtf -f0 -M6 -E0 $(TEXT).tex

clean:
	rm ms.md
	latexmk -c

continuous:
	while true; do make --silent; sleep 1; done
