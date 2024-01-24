# basic makefile for LaTeX/Markdown 
# 	uses latexmf for .tex files
# 	uses pandoc for .md files

TEXDOCS = $(patsubst %.tex,%.pdf,$(wildcard *.tex))
MDDOCS = $(patsubst %.md,%.pdf,$(wildcard *.md))
all: $(TEXDOCS) $(MDDOCS)
%.pdf: %.tex
	@latexmk -pdf -quiet -pdflatex="pdflatex -interaction=nonstopmode" $<
%.pdf: %.md
	@pandoc $< -o $@
clean:
	@latexmk -C -quiet
	@rm -f *.bm *.vrb *.nav *.snm *.synctex.gz
	@rm -f $(MDDOCS)