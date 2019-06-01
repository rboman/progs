

# Fortran + python

- j'utilise f2py fourni avec numpy.
- je compile sous windows avec msys2 / mingw64 / gfortran
- f2py s'installe : C:\msys64\mingw64\bin\f2py2.exe

intérêts de f2py ./. d'autres solutions:
- maintenu/utilisé par numpy => risque moins de disparaitre
- compatible numpy => interface np.array, matplotlib, scipy, etc.
- simple  - gère l'interface automatiquement
- pas de c++
- possibilité de compiler avec cmake

inconvénients:
- n'interface pas les types dérivés (c à d les structures)


## autres ressources
- http://calcul.math.cnrs.fr/Documents/Journees/dec2006/python-fortran.pdf
- regarder aussi https://github.com/mcfarljm/fortwrap (fortran=>C++=>swig?)
- https://github.com/jameskermode/f90wrap (couche au dessus de f2py)

