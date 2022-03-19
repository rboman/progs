# Utilisation de virtualenv

Créer l'environememnt virtuel
```
virtualenv project1  # crée un env. nommé "project1"
project1\Scripts\activate  # active l'environnement 
pip list             # affiche uniqut "pip, setuptools, wheel"
pip freeze --local  > requirements.txt
deactivate
```
Recréer l'environement virtuel
```
virtualenv project2  # crée un 2e env. 
project2\Scripts\activate  # active l'environnement 
pip install -r requirements.txt  # installe tous les packages
```
