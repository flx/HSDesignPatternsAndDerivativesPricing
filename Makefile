all: SimpleMC1.exec SimpleMC1b.exec SimpleMC2.exec SimpleMC3.exec \
	SimpleMC4.exec SimpleMC4b.exec SimpleMC4c.exec SimpleMC5.exec SimpleMC6.exec Main.pdf
	
exec: SimpleMC1.exec SimpleMC1b.exec SimpleMC2.exec SimpleMC3.exec \
	SimpleMC4.exec SimpleMC4b.exec SimpleMC4c.exec SimpleMC5.exec SimpleMC6.exec

Main.pdf: Main.tex SimpleMC1.lhs SimpleMC2.lhs SimpleMC3.lhs SimpleMC4.lhs SimpleMC5.lhs Payoff.lhs Option.lhs Parameter.lhs InputParams.lhs Parse.lhs Stats.lhs
	pdflatex Main.tex
	pdflatex Main.tex
	
clean:
	rm Main.tex Main.pdf
	rm *exec

.SUFFIXES: .lhs .mkd .html .tex .pdf .exec
 
PANDOC := pandoc --no-wrap -sS
HSCOLOUR := hscolour -lit
LHS2TEX := lhs2TeX
HASKELL := ghc
 
.lhs.mkd:
	cat $< | $(HSCOLOUR) -css > $@
 
.lhs.html:
	cat $< | $(HSCOLOUR) -css | $(PANDOC) -t html -c hscolour.css > $@
 
.lhs.tex:
	$(LHS2TEX) -o $@ $<
 
.lhs.exec:
	$(HASKELL) --make -threaded -O -o $@ $<  
 
.tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<