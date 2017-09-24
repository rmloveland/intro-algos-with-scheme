BOOKDIR=${HOME}/Dropbox/Documents/personal/intro-algos-with-scheme

all: html

html:
	confluence2html --image-directory $(BOOKDIR)/img/ --code-samples-directory $(BOOKDIR)/code/samples/ --stylesheet $(BOOKDIR)/data/style.css  < book.txt > book.html

clean:
	rm book.html

.PHONY: html
