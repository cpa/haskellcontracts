paper.pdf: paper.tex
	pdflatex paper.tex

checker:
	cd contracts && make
