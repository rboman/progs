# basic makefile for LaTeX (uses latexmf package)

ALLDOCS = $(patsubst %.tex,%.pdf,$(wildcard *.tex))
all: $(ALLDOCS)
%.pdf: %.tex
	@latexmk -pdfps -quiet -latex="latex -interaction=nonstopmode %O %S" $<
clean:
	@latexmk -C -quiet
	@rm -f *.bm *.vrb
