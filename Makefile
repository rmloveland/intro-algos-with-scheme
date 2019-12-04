all: html

html:
	confluence2html < book.txt > book.html

clean:
	rm *.html
