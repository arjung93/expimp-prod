SHELL :=//bin/bash
all:
	latexmk -pdf proposal.tex

clean:
	rm -f *.{aux,log,nav,out,snm,synctex.gz,tex~,toc,dvi,bbl,blg,glo,gls,acr,glg,alg,acn,ist,bak,lot,lof,fls,run.xml,bcf,fdb_latexmk}; \
	rm -r auto/

squeaky:
	-rm proposal.pdf
slides: 
	latexmk -pdf sl_defence.tex
