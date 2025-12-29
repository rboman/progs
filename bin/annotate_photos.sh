#!/usr/bin/env bash
#
# ChatGPT:
#   """j ai une série d'images nommées "AAAA-MM-DD - Texte.jpg". 
#   J'aimerais écrire "Texte (DD "Mois en lettres" AAAA)" 
#   dans un coin de chaque photo."""
# 
# sudo apt install imagemagick
#   magick -version    (>=7)
#   convert -version   (<7)
#
# config langue FR:
#   sudo apt update
#   sudo apt install locales
#   sudo locale-gen fr_FR.UTF-8
#   sudo update-locale
#   locale -a | grep fr_FR     => doit afficher "fr_FR.utf8"
#
#   export LC_TIME=fr_FR.UTF-8

set -euo pipefail

outdir="annotated"
mkdir -p "$outdir"

shopt -s nullglob
for f in *.jpg *.JPG *.jpeg *.JPEG *.png *.PNG; do
  base="$(basename "$f")"

  # attend "AAAA-MM-DD - Texte.ext"
  date_part="${base%% - *}"
  texte_part="${base#* - }"
  ext="${base##*.}"
  texte_part="${texte_part%.*}"

  # extraction AAAA MM DD
  AAAA="${date_part:0:4}"
  MM="${date_part:5:2}"
  DD="${date_part:8:2}"

  # mois en lettres (Linux GNU date) + fallback macOS
  if mois=$(LC_TIME=fr_FR.UTF-8 date -d "$AAAA-$MM-$DD" +"%B" 2>/dev/null); then
    :
  else
    mois=$(LC_TIME=fr_FR.UTF-8 date -j -f "%Y-%m-%d" "$AAAA-$MM-$DD" +"%B")
  fi

  annotation="$texte_part ($DD $mois $AAAA)"
  out="$outdir/$base"

  # changer en "magick" si imagemagick >=7

  convert "$f" \
    -auto-orient \
    +set exif:Orientation \
    -gravity SouthEast \
    -fill white \
    -undercolor '#00000080' \
    -pointsize 100 \
    -annotate +20+20 "$annotation" \
    "$out"
done

echo "✅ Terminé. Images annotées dans: $outdir/"
