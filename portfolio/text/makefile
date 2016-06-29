all: supp.pdf

%.pdf: %.rmd
	echo "library(rmarkdown); render(\"$*.rmd\")" | R --slave

%.pdf: %.Rnw
	echo "library(knitr); knit2pdf(\"$*.Rnw\")" | R --slave

%.pdf: %.tex
	latexmk $*.tex

%.tex: %.Rnw
	echo "library(knitr); knit(\"$*.Rnw\")" | R --slave
