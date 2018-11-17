all: pdf

pdf:
	latexmk -f -quiet -pdf book.tex

clean:
	rm *.aux *.dvi *latexmk *.log *.fls
