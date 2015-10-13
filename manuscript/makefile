all:
	pdflatex -interaction=nonstopmode -halt-on-error paper.tex
	bibtex *.aux
	pdflatex -interaction=nonstopmode -halt-on-error paper.tex
	pdflatex -interaction=nonstopmode -halt-on-error paper.tex

clean:
	rm *.aux *.bbl *.blg *.dvi *.synctex.gz *.log
