FILE = lacl11

all:
	pdflatex $(FILE).tex
	
short:
	cp $(FILE).tex x
	sed -e  's/\\newif\\iffullversion\\fullversiontrue/\\newif\\iffullversion\\fullversionfalse/g' x > $(FILE)-short.tex
	pdflatex $(FILE)-short.tex 
	rm x

long:
	cp $(FILE).tex x
	sed -e  's/\\newif\\iffullversion\\fullversionfalse/\\newif\\iffullversion\\fullversiontrue/g' x > $(FILE)-long.tex
	pdflatex $(FILE)-long.tex 
	bibtex $(FILE)-long
	pdflatex $(FILE)-long.tex
	rm x
                                                                                	