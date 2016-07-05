TEX = pdflatex -interaction=nonstopmode -file-line-error

all : folien arbeitsblatt
	echo $(MAKE_VERSION)

clean :
	rm -f *.aux arbeitsblatt.pdf folien.pdf folien.sty *.log *.nav *.out *.snm *.toc *.vrb

folien : folien.tex
	$(TEX) folien.tex

arbeitsblatt : arbeitsblatt.tex
	$(TEX) arbeitsblatt.tex
