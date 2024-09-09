# winlibs

Mise en place de l'environnement de libs sur une machine

```
winlibs.py update   # <= dézippe les libs vers C:\local
winlibs.py link     # <= crée des liens symboliques
set_libenv.py       # <= enrichit LIB, INCLUDE, etc. (sauf PATH) - utilise setx
add2path.cmd        # <= enrichit le PATH - utilise pathed
```

Editer le PATH avec Path Editor pour vérifier / nettoyer

* Python310 est placé par l'installeur dans le PATH système et non le PATH user
* dans le PATH système, pour OneAPI, je supprime
    * les libs 32bits
    * tout ce qui a rapport à Intel MPI
    * il reste 1 entrée compiler, 1 entrée tbb et une entrée MKL
* pour moi: ajouter dev/progs/bin au PATH
