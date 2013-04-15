#! /bin/bash

rm -f *.aux *.log *.toc *.bbl *.blg *~
cd preliminar
sh clean.sh
cd ../principal
sh clean.sh
