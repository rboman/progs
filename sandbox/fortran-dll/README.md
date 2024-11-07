# Essai dlls avec fortran

win/linux & gfortran/ifort



## dllexport?

En gfortran, tous les symboles sont exportés qu'on soit sous linux (ca c'est habituel), mais aussi sous windows (!!)

Résultat: il n'y a pas besoin de directive spécifique de type "dllexport" à gfortran quand on crée une dll. Ca marche comme sous linux!

Pour le compilateur intel, en revanche, `!dec$ attributes dllexport` est requis sous windows (ou `!dir$` ...)

Autre info: `!dec$ attributes dllimport` ne semble jamais utile ni pour intel ... et encore moins pour gfortran...

## symboles exportés?

Dans une ligne de commande msvc, ou avec `C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\bin\Hostx64\x64\dumpbin.exe`

* win / gfortran msys2:

````
dumpbin /EXPORTS libaddition.dll
````

````
  Section contains the following exports for libaddition.dll
...
          1    0 000013B0 __module1_MOD_addition_in_mod
          2    1 00001410 __stringlist_MOD_print_stringlist
          3    2 00001380 addition_
          4    3 000013E0 addition_bindc
````

* win / ifort:

````
Dump of file Release\addition.dll
...
          1    0 00001000 ADDITION
          2    1 00001120 MODULE1_mp_ADDITION_IN_MOD
          3    2 00001020 STRINGLIST_mp_PRINT_STRINGLIST
          4    3 00001140 addition_bindc
````

* ubuntu  gfortran

````
objdump -T libaddition.so
````

````
libaddition.so:     file format elf64-x86-64
...
0000000000001185 g    DF .text  000000000000002e  Base        addition_bindc
00000000000011b3 g    DF .text  0000000000000111  Base        __stringlist_MOD_print_stringlist
0000000000001157 g    DF .text  000000000000002e  Base        __module1_MOD_addition_in_mod
0000000000001129 g    DF .text  000000000000002e  Base        addition_
````

