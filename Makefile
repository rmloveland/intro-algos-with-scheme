all: pdf

open: pdf
	open -a Preview book.pdf

pdf:
	latexmk -quiet -pdf book.tex

clean:
	rm *.aux *.dvi *latexmk *.log *.fls
