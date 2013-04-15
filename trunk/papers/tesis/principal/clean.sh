#!/bin/bash

rm -f *.aux *.log *.toc *.pdf *.bbl *.blg *~

for p in introduccion nlg comp_instructor grounding evaluacion conclusiones
do
	rm -f $p/*.aux $p/*.log $p/*.toc $p/*.pdf $p/*.bbl $p/*.blg $p/*~
done

