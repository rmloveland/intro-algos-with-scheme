all: pdf

pdf:
	latexmk -quiet -pdf book.tex

clean:
	rm *.aux *.dvi *latexmk *.log *.fls
