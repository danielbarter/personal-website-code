cabal build
gen-hie > hie.yaml
cabal exec personal-website-code
cd ./content/tex
pdflatex danielbarter.tex > /dev/null
cp danielbarter.pdf ../../site/CV.pdf
cd ../../
