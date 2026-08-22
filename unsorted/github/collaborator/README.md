# GitHub Collaborations Report

➡️ [github-repositories.html](https://raw.githack.com/rboman/progs/master/unsorted/github/collaborator/github-repositories.html)

Petit outil Python autonome qui génère une page HTML statique répertoriant les
repositories externes auxquels le compte GitHub authentifié collabore.

Prérequis : Python 3.11 ou plus récent et un token GitHub disponible dans la
variable d'environnement `GITHUB_TOKEN`. Préférez un *fine-grained personal
access token* limité aux seuls repositories et permissions nécessaires. Le
script n'affiche pas le token et ne l'écrit jamais dans le rapport HTML.

## Installation et utilisation sous Linux

```bash
python -m pip install -r requirements.txt
export GITHUB_TOKEN=github_pat_xxx
python github_collaborations.py
```

## Installation et utilisation sous Windows (`cmd.exe`)

```bat
python -m pip install -r requirements.txt
set GITHUB_TOKEN=github_pat_xxx
python github_collaborations.py
```

Le rapport est créé par défaut dans `github-repositories.html`. Ouvrez ce
fichier directement dans votre navigateur ; aucun serveur HTTP n'est requis.
Pour choisir un autre emplacement :

```bash
python github_collaborations.py --output mon-rapport.html
```

Le script interroge l'API REST GitHub, suit automatiquement toutes les pages,
exclut les repositories appartenant au compte authentifié, puis génère un
fichier autonome contenant son CSS et son JavaScript. La recherche et les
filtres de permission et de visibilité fonctionnent entièrement dans le
navigateur, sans connexion supplémentaire.

En cas d'erreur d'authentification ou d'API, le programme affiche un message
concis et se termine avec un code non nul.
