@echo off
setlocal enabledelayedexpansion

REM contexte: Suppression de la partie "animee" de fichiers JPG en réécrivant
REM les images avec ImageMagick. Il s'agit de "mini MP4" ajoutés dans le JPG.
REM Utile pour les JPG issus de captures d'écran de vidéos (cam dji)
REM On demande à ImageMagick de réécrire le JPG (il ignore la video)

REM === ImageMagick ancien ===
set IM="c:\local\ImageMagick\convert.exe"

REM === Dossier de sortie ===
set OUTDIR=jpg_propres

if not exist "%OUTDIR%" mkdir "%OUTDIR%"

echo Re-ecriture propre des JPG (compatible IM ancien)...
echo.

for %%F in (*.jpg *.JPG) do (
    echo Traitement : %%F
    %IM% "%%F" ^
        -auto-orient ^
        -define jpeg:preserve-settings=true ^
        "%OUTDIR%\%%F"
)

echo.
echo Terminé.
echo Les JPG propres sont dans le dossier "%OUTDIR%".
pause
