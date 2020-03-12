#!/bin/bash

stack run
cd ./content/tex
pdflatex danielbarter.tex > /dev/null
cp danielbarter.pdf ../../site/CV.pdf
cd ../../
