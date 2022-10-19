# HTML
pandoc .pandoc-config.yml article.md -o article.html --standalone --mathjax -F pandoc-crossref --citeproc
# PDF
pandoc .pandoc-config.yml article.md -o article.pdf --standalone -F pandoc-crossref --citeproc