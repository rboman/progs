
@echo off
REM Création de l'environnement virtuel s'il n'existe pas
IF NOT EXIST .venv (
    echo Création de l'environnement virtuel...
    python -m venv .venv
)

REM Activation de l'environnement virtuel
call .venv\Scripts\activate

REM Installation des dépendances
echo Installation des dépendances...
pip install -r requirements.txt

REM Exécution du script Python
echo Exécution du script...
python analyse_visa.py

REM Pause pour laisser le terminal ouvert
pause
