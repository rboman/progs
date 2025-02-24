# HTML
pandoc .pandoc-config.yml \
       article.md -o article.html \
       --standalone \
       --mathjax=https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js \
       -F pandoc-crossref --citeproc
# PDF
pandoc .pandoc-config.yml \
       article.md -o article.pdf \
       -t markdown+emoji \
       --pdf-engine=xelatex  -V mainfont="DejaVu Sans" \
       -F pandoc-crossref --citeproc