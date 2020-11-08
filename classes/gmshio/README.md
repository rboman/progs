gmshio
======

[![Build Status](https://travis-ci.org/rboman/gmshio.svg?branch=master)](https://travis-ci.org/rboman/gmshio)

Fichiers relatifs aux cours #1 du Projet de code scientifique multiphysique
"comment écrire un code de plus de 100 lignes?"

- geo: contient le cube (fichier gmsh et le maillage resultant)
- v1: 'gmshio.cpp' basique
- v2: solution de l'exercice propose

# compilation/run
```
cd v1
mkdir build
cmake -A x64 ..
cmake --build . --config Release
Release\solver.exe ..\..\geo\cube22.msh
```
Result:
```
Reading 2197 nodes
Reading 1872 elements
Read 2197 nodes and 1872 elements
```
