# Memo Vim

## Vidéos

- ThePrimeagen: https://www.youtube.com/watch?v=n9k9scbTuvQ
- DistroTube: https://www.youtube.com/watch?v=ER5JYFKkYDg


## Configuration

- cfg dans `~/.vim/`
- editer `.vimrc`
- `:source %`                             : source the current file (utile pour `.vimrc`)
- `mkdir ~/.vimrc/undodir -p`   : make undodir (-p = recursive)

## Plugins

https://github.com/junegunn/vim-plug

mettre `plug.vim` dans `~/.vim/autoload`:

```
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```
définir des plugins dans une section `call plug#begin` / `call plug#end`

Ensuite:

```
:w
:source %
:PlugInstall
```

Quelques plugins:

- [nerdtree: A tree explorer plugin for vim.](https://github.com/preservim/nerdtree)
- [ctrlp.vim: Fuzzy file finder.](https://github.com/kien/ctrlp.vim)
- [vim-fugitive: fugitive.vim: A Git wrapper](https://github.com/tpope/vim-fugitive)
- [syntastic: Syntax checking hacks for vim](https://github.com/vim-syntastic/syntastic)


## Tutoriels

- `vimtutor` : tutoriel dans un terminal (commande installée avec vim)
- https://vim-adventures.com/

## Commandes

### EX MODE: cmds:

| commande      | effet                                       |
| ------------- | ------------------------------------------- |
| `:q :x ZZ`    | quitter                                     |
| `:w`          | sauver                                      |
| `:! ls`       | lance `ls` puis revient dans vim            |
| `:help split` | aide sur le thème `split`                   |
| `:help ^W`    | aide sur CTRL-W                             |
| `:<Arrow up>` | history                                     |
| `:Ex<Tab>`    | completion                                  |
| `:e <Ctrl>d`  | montre toutes les cmds qui débutent par `e` |

### NORMAL MODE: mouvements:

| commande  | effet                                                        |
| --------- | ------------------------------------------------------------ |
| `h,j,k,l` | déplacement                                                  |
| `2j`      | déplacement x2                                               |
| `gg`      | ligne 0                                                      |
| `G`       | dernière ligne                                               |
| `5G`      | va à la ligne #5                                             |
| `^ ou 0`  | début de ligne                                               |
| `$`       | fin de ligne                                                 |
| `w`       | [word] avance au début du mot suivant (ponctuation=mot)      |
| `e`       | [end] avance à la fin du mot (ponctuation=mot)               |
| `b`       | [backwards] recule au début du mot précédent                 |
| `W, E, B` | same as `w`, `e`, `b` but ponctuation=character              |
| `5w`      | move 5 words forward                                         |
| `f...`    | [forward]                                                    |
| `f(`      | forward to `(`                                               |
| `F(`      | backward to `(`                                              |
| `t(`      | forward unTil (` (cursor before `(`)                         |
| `T(`      | backward unTil `(`                                           |
| `%`       | cherche les parenthèses dans lesquelles le curseur se trouve |
| `<Ctrl>o` | renvoie où on était avant un saut                            |
| `<Ctrl>i` | retourne où on était avant un `<Ctrl>o`                      |
| `<Ctrl>f` | scroll forwards (1 page)                                     |
| `<Ctrl>b` | scroll backwards (1 page)                                    |
| `L,M,H`   | move cursor to the bottom, middle, top of the page           |
| `<Ctrl>e` | scroll down (1 line without moving cursor)                   |
| `<Ctrl>y` | scroll up (1 line without moving cursor)                     |

### NORMAL MODE: undo, delete

| commande    | effet                                                        |
| ----------- | ------------------------------------------------------------ |
| `u`         | undo                                                         |
| `<Ctrl>r`   | redo                                                         |
| `d[+mouvt]` | delete  (contenu dans le presse-papier)                      |
| `dd`        | delete curent line                                           |
| `dw`        | [del-word] delete current word from cursor to the beg of next word |
| `de`        |                                                              |
| `d$`        | delete until the end of line (content can be pasted with `y`) |
| `5dd`       | delete 5 lignes                                              |
| `D`         | = d$                                                         |

### NORMAL MODE: copy (yank) / paste (put)

| commande           | effet                              |
| ------------------ | ---------------------------------- |
| `y[+mouvt]`        | [yank] copy                        |
| `yy`               | copy current line                  |
| `yw, ye, y$, etc.` | similaire à `dw`, `de`, `d$`, etc. |
| `p`                | "put" after the cursor             |
| `P`                | "put" before the cursor            |

### SEARCH / SEARCH-REPLACE

| commande             | effet                                                        |
| -------------------- | ------------------------------------------------------------ |
| `/text`              | search downwards '`text`'                                    |
| `?text`              | search upwards `text`, then use `n` or `N` to navigate forwards/backwards |
| `:%s/text1/text2/g`  | remplace `text1` par `text2`                                 |
| `:%s/text1/text2/gc` | idem mais demande confirmation                               |


### INSERT MODE

| commande  | effet                                                        |
| --------- | ------------------------------------------------------------ |
| `i`       | insert                                                       |
| `I`       | insert at the first non blank char of the line               |
| `a`       | append (insert after the cursor)                             |
| `A`       | append at the end of the line (= `$a`)                       |
| `s`       | delete the current character + insert                        |
| `S`       | delete the current line + insert                             |
| `o`       | insert a line after the current line                         |
| `O`       | insert a line before the current line                        |
| `c[+mvt]` | [change] as `i` but wait for a motion (content can be pasted with `y`) |
| `cw`      | change the text from the cursor to the beg of next word      |
| `C`       | = c$                                                         |


### VISUAL MODE

| commande   | effet        |
| ---------- | ------------ |
| `v`        | visual mode  |
| `V`        | visual line  |
| `<Ctrl>-v` | visual block |

Expl: commenter des lignes:
- `<Ctrl>-v` : visual block
- aller à la derniere ligne à commenter
- `I` : insert (pas `i`)
- `"<Esc>` : ajoute des guillemets  (`//<Esc>` pour du C++)


### Splits (multi-fenêtres)

| commande                 | effet                                   |
| ------------------------ | --------------------------------------- |
| `:vs`   (ou `:wincmd v`) | split vertical (2 fenêtres cote à cote) |
| `:vertical resize 30`    | taille en caracteres                    |
| `<Ctrl>w-w`              | move cursor to other window             |
| `<Ctrl>w-<Ctrl>-w`       | idem                                    |
| `<Ctrl>w-r`              | swap windows                            |
| `:q`                     | quitte le split courant                 |
| `<Ctrl>w-J`              | split vertical => split horiz           |
| `<Ctrl>w-H`              | split horiz => split vertical           |


### Explorer (netrw)

| commande              | effet                                        |
| --------------------- | -------------------------------------------- |
| `:Ex :Explore`        | ouvre l'explorateur (plugin `netrw` de base) |
| `:Sexplore`           | idem dans un Hsplit                          |
| `:Vexplore`           | idem dans un Vsplit                          |
| `i` (dans l'explorer) | change le mode                               |


## Advanced Editing

`{command}{text object or motion} `

### commands

- `d`   : delete (cut)
- `c`   : change (cut+insert)
- `y`   : yank (copy)
- `v`   : visually select

### text objects:
- `w`   : word
- `s`   : sentence
- `p`   : paragraph
- `t`   : tag 

### motions

- `a`   : all
- `i`   : in
- `t`   : 'til
- `f`   : find forward
- `F`   : find backward

### Examples

`diw`         : delete in word
`daw`         : delete in word including space 
`ciw`         : change in word
`yi) = yi(`   : yank in parenthesis
`ya)`         : yank in parenthesis including ')'
`dt,`         : delete 'til comma
`df,`         : delete 'til comma including comma
`va"`         : visually select everything inside " (including ")

### Dot Command

`.`   : repeat the last command

*expl:* 

- `ciwblabla<Esc>`  then '`.`'
- `I//<Space><Esc>` then '`.`'
- `3d<Space>`       then '`.`'

### Macros

*Record macro:*

- `q{register}`     : hint: utiliser `qq` si on a besoin d'une seule macro
- (do things)
- `q`

*Play macro:*

- `@{register}`

*Play macro sur une sélection:*

- `V`    (select)
- `:normal @{register}`

### Bookmarks (utiles pour macros)

- `m{key}`  : store position in mark named `{key}i` (hint: utiliser `mm` si 1 mark)
- ``{key}`  : go back to position `{key}`

### Registers

- `:reg`    : see registers (includes clipboard, macros)
- `"5p`     : rappelle le registre `5` et paste











