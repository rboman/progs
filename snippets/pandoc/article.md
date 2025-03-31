---
title: My Markdown article
date: \today
lang: fr
# ...citeproc
bibliography: zotero.bib
csl: materials-technology.csl
link-citations: true
# ...pandoc-crossref
linkReferences: true
nameInLink: true
# ...pdf variables
papersize: a4
margin-left: 2cm
margin-right: 2cm
colorlinks: true
# ... autre
numbersections: true
autoEqnLabels: true
---

<!-- je n'utilise pas .pandoc-config.yml ici pour avoir le bon affichage dans VSCode sans devoir configurer globalement les options -->


# Notes Pandoc

Petits tests divers en regardant cette video: [Video](https://youtu.be/J86Pm62XM_Q)

* [Pandoc User’s Guide](https://pandoc.org/MANUAL.html)
* [pandoc-crossref](https://lierdakil.github.io/pandoc-crossref/)

## Extensions VSCode

* [markdown-preview-enhanced](https://shd101wyy.github.io/markdown-preview-enhanced/#/)

Settings:

* activer "use pandoc parser" => les figures & captions apparaissent.
* ajouter les settings:

```
    "markdown-preview-enhanced.pandocArguments": [
        "-F","pandoc-crossref"
    ],
```

Voir aussi:
* [Pandoc Citer - Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=notZaki.pandocciter): permet d'avoir la liste des refs quand on écrit [@]

## Paragraphes bidons

Commodo cillum non sint tempor. Duis laboris qui ipsum laboris sint ut occaecat duis enim magna Lorem dolor est tempor. Consectetur ea veniam duis consequat sunt. Deserunt dolore elit **exercitation** fugiat. Elit incididunt aliqua ullamco Lorem nostrud et eu enim minim id do cupidatat nulla. Qui nostrud culpa quis reprehenderit ex excepteur excepteur exercitation est non enim dolor fugiat.

Exercitation est Lorem deserunt adipisicing ipsum enim proident. Anim est adipisicing nostrud commodo. Irure _cupidatat_ consectetur duis duis adipisicing voluptate esse aliquip. Velit enim consectetur occaecat elit pariatur. Proident id reprehenderit enim ex cillum aliquip ex consequat culpa in irure. Eiusmod enim sint culpa adipisicing duis aliquip consectetur duis in ex minim incididunt pariatur.

![This is a kitten](placekitten.jpg){#fig:kitten}

Officia laborum ipsum aliquip id sint elit sunt sint cillum reprehenderit. Tempor aliqua veniam velit laborum irure. Irure deserunt voluptate nostrud quis. Occaecat occaecat aliqua ipsum voluptate sit duis dolor deserunt culpa enim do ex. Minim aliqua non irure sit ex et non culpa elit aute ipsum irure incididunt qui. Velit eiusmod est exercitation ut ullamco id duis et nulla.

## Footnotes

Hello[^1]
Another way to put the footnote[^Hello]

[^1]: This is a footnote
[^Hello]: This is the second footnote

## Citations

Nécessite `bibliography` dans le YAML frontmatter (et "use pandoc" dans les settings de "Markdown Preview Enhanced")

Citations: [@boman_efficient_2012], [@boman_application_2006], [@boman_numerical_2002]

Styles: [Zotero Style Repository](https://www.zotero.org/styles)

Le filtre "citeproc" doit être utilisé par que la biblio apparaisse quand on utilise la ligne de commande.

Le filtre externe "pandoc-citeproc" n'existe plus dans les nouvelles versions de pandoc. Remplacé par `--citeproc`
```
pandoc article.md -o article.html --standalone --citeproc 
```
Utiliser `link-citations: true` dans le frontmatter pour avoir les liens entre la biblio et les citations.

## cross-ref {#sec:crossref}

Utiliser {#nom} et installer [pandoc-crossref](https://github.com/lierdakil/pandoc-crossref/releases) (vérifier version identique à pandoc!)

voir [@fig:kitten]

```
pandoc article.md -o article.html --standalone -F pandoc-crossref --citeproc
```
utiliser:
```
linkReferences: true        # cree les liens
nameInLink: true            # inclut "Fig" dans le lien
```
Référence vers @sec:crossref

## Au delà

[Live Server - Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=ritwickdey.LiveServer)

Clic droit sur le fichier html et faire "Open with Live server". Ca démarre un serveur web sur le port 5500 qui va se rafraichir chaque fois que le fichier change.


## pdf

Même ligne de commande!
```
pandoc article.md -o article.pdf --standalone -F pandoc-crossref --citeproc
```
Ajouter:
```
papersize: a4
margin-left: 2cm
colorlinks: true
```
Possibilité de faire: `pandoc -V margin-left=2cm ...`

## equations

$$
a^2 = b^2+c^2
$$

$$
\sin(\frac{\pi}{2})=1
$$ {#eq:sinus}

See @eq:sinus

## Bibliography