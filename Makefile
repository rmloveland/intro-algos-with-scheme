all: html

html:
	confluence2html --image-directory ~/Documents/personal/intro-algos-with-scheme/img/ --code-samples-directory ~/Documents/personal/intro-algos-with-scheme/code/ --stylesheet ~/Documents/personal/intro-algos-with-scheme/data/style.css  < book.txt > book.html

clean:
	rm book.html
