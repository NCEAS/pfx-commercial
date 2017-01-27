TEXT = anderson-etal-rev-pfx
MS = ms
SOM = supp
REFS = refs

all: $(TEXT).pdf $(SOM).pdf

%.md: %.Rmd
	cp $< temp.Rmd
	perl -p -i -e "s/ \`r\n/ \`r \n/g" temp.Rmd # add space at end for in-line R code 
	perl -p -i -e "s/\(\`r\n/ \(\`r \n/g" temp.Rmd # add space at end for in-line R code
	Rscript -e "knitr::knit('temp.Rmd', output = '$(basename $<).md')"
	rm temp.Rmd

%.tex: %.Rnw $(REFS).bib
	Rscript -e "knitr::knit('$<')"

%.tex: %.md $(REFS).bib
	pandoc -S --wrap=none --bibliography=$(REFS).bib --natbib -fmarkdown-all_symbols_escapable $< -o $@
	perl -p -i -e "s/Figure /Figure~/g" $@
	perl -p -i -e "s/Fig\. /Fig.~/g" $@
	perl -p -i -e "s/vs\. /vs.~/g" $@
	perl -p -i -e "s/Table /Table~/g" $@
	perl -p -i -e "s/Figs\. /Figs.~/g" $@
	perl -p -i -e "s/\\\citep/\\\cite/g" $@
	perl -p -i -e "s/\\\citet/\\\cite/g" $@

$(TEXT).pdf: $(MS).tex $(TEXT).tex $(REFS).bib
	latexmk -pdf $(TEXT)

$(SOM).pdf: $(SOM).tex $(REFS).bib
	latexmk -pdf $(SOM)

dropbox: $(TEXT).pdf $(SOM).pdf $(TEXT).rtf
	cp $(TEXT).pdf ~/Dropbox/Public/$(TEXT).pdf
	cp $(SOM).pdf ~/Dropbox/Public/$(SOM).pdf
	cp $(TEXT).rtf ~/Dropbox/Public/$(TEXT).rtf

rtf: $(TEXT).pdf
	latex2rtf -f0 -M3 -E0 $(TEXT).tex
	# latex2rtf $(TEXT).tex

clean:
	latexmk -c
	rm -f $(SOM).tex
	rm -f $(MS).tex
	rm -f $(MS).md
	rm -f $(SOM).pdf
	rm -f $(TEXT).pdf

continuous:
	while true; do make --silent; sleep 1; done

touch:
	touch $(MS).Rmd
	touch $(SOM).Rnw
	touch $(TEXT).tex

