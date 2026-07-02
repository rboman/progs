# progs

[![build-test](https://github.com/rboman/progs/actions/workflows/build-test.yml/badge.svg)](https://github.com/rboman/progs/actions/workflows/build-test.yml)
[![Apache License Version 2.0](https://img.shields.io/badge/license-Apache_2.0-green.svg)](LICENSE)

Various progs of mine:
  * `apps`: small almost-finished apps,
  * `bin`: helper scripts,
  * `docs`: misc text / markdown tests,
  * `envs`: build environment scripts,
  * `externals`: place to download/build external dependencies,
  * `ico`: shared icons and images,
  * `metafor`: programs related to [Metafor](http://metafor.ltas.ulg.ac.be/),
  * `pytools`: shared python tools,
  * `sandbox`: tutorials I wrote during the learning of a new language/library,
  * `skel`: skeleton of progs including CMake files,
  * `snippets`: useful code fragments,
  * `student`: my old student projects,
  * `unsorted`: unsorted projects.

## Navigation

This repository is intentionally broad and historical. For a more detailed map:

* [Repository map](docs/REPOSITORY_MAP.md)
* [Project catalog](docs/PROJECT_CATALOG.md)
* [AI navigation guide](docs/AI_NAVIGATION.md)
* [Reorganization notes](docs/REORGANIZATION_NOTES.md)

AI coding agents should also read [AGENTS.md](AGENTS.md) before making changes.

## Memo rapide: utiliser ces scripts avec pipx

Si vous ne connaissez pas `pipx`, retenez ceci: `pipx` installe les outils Python CLI dans un environnement isole, tout en rendant les commandes disponibles dans votre shell.

### 1) Installer pipx (une seule fois)

```bash
python3 -m pip install --user pipx
python3 -m pipx ensurepath
```

Fermez/reouvrez le terminal (ou rechargez votre shell), puis verifiez:

```bash
pipx --version
```

### 2) Installer les scripts de ce repo

Depuis la racine de ce repo:

```bash
cd /chemin/vers/progs
pipx install -e .
```

Le `-e` (editable) est pratique ici: si vous modifiez le code des scripts, les commandes `rb-*` utilisent directement votre copie locale.

### 3) Utiliser une commande (exemple)

```bash
rb-updateoffi
```

### 4) Mettre a jour apres un `git pull`

En general, rien a faire. Si besoin de reinitialiser l'environnement pipx:

```bash
pipx uninstall rboman-progs
pipx install -e .
```
