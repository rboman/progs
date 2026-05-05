#!/usr/bin/env bash

# ============================================================
# Conversion de plusieurs fichiers Markdown en un seul PDF
# avec saut de page entre chaque fichier.
#
# Dépendances :
#   - pandoc
#   - xelatex (ou autre moteur LaTeX)
#
# Utilisation :
#   ./md2pdf.sh
#   ./md2pdf.sh output.pdf
# ============================================================

# =========================
# Paramètres (modifiables)
# =========================

OUTPUT="${1:-sortie.pdf}"   # Nom du fichier PDF de sortie
INPUT_PATTERN="*.md"       # Pattern des fichiers Markdown

FONT_SIZE="12pt"            # Taille de police (ex: 12pt, 16pt, 20pt)
MAIN_FONT="TeX Gyre Heros"  # Police sans serif (ex: TeX Gyre Heros, Liberation Sans)
MONO_FONT="DejaVu Sans Mono" # Police pour les blocs de code

PAPER_SIZE="a4"             # Format papier
ORIENTATION="landscape"     # landscape ou portrait
MARGIN="2cm"                # Marges

# =========================
# Vérifications
# =========================

if ! command -v pandoc >/dev/null 2>&1; then
  echo "Erreur : pandoc n'est pas installé."
  exit 1
fi

# =========================
# Construction du document
# =========================

TMP_FILE=$(mktemp)

first=1
for f in $(ls $INPUT_PATTERN | sort -V); do
  if [ "$first" -eq 0 ]; then
    echo "" >> "$TMP_FILE"
    echo '```{=latex}' >> "$TMP_FILE"
    echo "\\clearpage" >> "$TMP_FILE"
    echo '```' >> "$TMP_FILE"
    echo "" >> "$TMP_FILE"
  fi
  cat "$f" >> "$TMP_FILE"
  first=0
done

# =========================
# Conversion avec Pandoc
# =========================

pandoc "$TMP_FILE" \
  -f markdown \
  -o "$OUTPUT" \
  --pdf-engine=xelatex \
  -V documentclass=extarticle \
  -V fontsize="$FONT_SIZE" \
  -V mainfont="$MAIN_FONT" \
  -V sansfont="$MAIN_FONT" \
  -V monofont="$MONO_FONT" \
  -V papersize="$PAPER_SIZE" \
  -V geometry:"$ORIENTATION" \
  -V geometry:margin="$MARGIN"

# =========================
# Nettoyage
# =========================

rm "$TMP_FILE"

echo "PDF généré : $OUTPUT"
