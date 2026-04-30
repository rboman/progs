# 07 — Système de Build (CMake)

---

## BUILD-01 🟠 Deux projets CMake séparés pour Fortran et C++

Le code C++ et le code Fortran ont des projets CMake **entièrement séparés** :

- `CMakeLists.txt` (racine) → projet `LOUIS_CPP` → compile `sph.dll` + `sphw.pyd`
- `src_f/CMakeLists.txt` → projet **standalone** `LOUIS_F` → compile `louis.exe`

Le projet Fortran est un sous-projet standalone (`CMAKE_MINIMUM_REQUIRED` et `PROJECT` redéfinis), ce qui signifie qu'il ne peut pas être build comme sous-répertoire du projet principal sans conflits. La commande `ADD_SUBDIRECTORY(src_f)` dans le CMakeLists racine inclut ce projet standalone comme sous-dossier, ce qui est une utilisation non standard de CMake.

En pratique, le projet Fortran est buildé **indépendamment** depuis son propre répertoire (`src_f/`), et le CMakeLists racine l'inclut sans que les deux projets partagent des cibles.

---

## BUILD-02 🟠 Recherche manuelle d'Eigen3 au lieu de `find_package`

**Fichier** : [sph/src/CMakeLists.txt](../sph/src/CMakeLists.txt)

```cmake
# FIND_PACKAGE(Eigen3 3.4 REQUIRED NO_MODULE)  ← commenté
FIND_PATH(EIGEN_INCLUDE_DIRS "Eigen/Dense"
          PATHS "${PROJECT_SOURCE_DIR}/lib/eigen"
                "/usr/include/eigen3"
                "c:/msys64/mingw64/include/eigen3" REQUIRED)
TARGET_INCLUDE_DIRECTORIES(${LIBNAME} PUBLIC ${EIGEN_INCLUDE_DIRS})
# TARGET_LINK_LIBRARIES(${LIBNAME} Eigen3::Eigen)  ← commenté
```

La recherche moderne d'Eigen3 via `find_package(Eigen3 REQUIRED NO_MODULE)` est commentée. Le `FIND_PATH` manuel avec des chemins hardcodés (`c:/msys64/mingw64/include/eigen3`) est fragile et non portable. Si Eigen3 est installé dans un autre chemin, la build échoue même si Eigen3 est disponible via CMake.

**Solution** : Activer la ligne `FIND_PACKAGE(Eigen3 3.4 REQUIRED NO_MODULE)` et utiliser `Eigen3::Eigen` comme cible importée.

---

## BUILD-03 🟠 `FILE(GLOB SRCS *.h *.cpp)` : recompilation partielle défaillante

**Fichier** : [sph/src/CMakeLists.txt](../sph/src/CMakeLists.txt)

```cmake
FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)
ADD_LIBRARY(${LIBNAME} SHARED ${SRCS})
```

`FILE(GLOB ...)` est déconseillé par la documentation CMake officielle car les fichiers ajoutés ou supprimés ne sont pas détectés sans relancer CMake. Un `cmake --build` seul ne re-générera pas les Makefiles si un fichier a été ajouté.

**Solution** : Lister les sources explicitement :
```cmake
ADD_LIBRARY(sph SHARED
    sph.cpp
    sphModel.cpp
    sphParticle.cpp
    sphFixedParticle.cpp
    sphMobileParticle.cpp
    sphSorter.cpp
    sphKernels.cpp
    sphTimer.cpp
)
```

---

## BUILD-04 🟠 Chemin hardcodé Windows dans le CMakeLists

**Fichier** : [sph/src/CMakeLists.txt](../sph/src/CMakeLists.txt)

```cmake
FIND_PATH(EIGEN_INCLUDE_DIRS "Eigen/Dense"
          PATHS "c:/msys64/mingw64/include/eigen3")
```

Le chemin `c:/msys64/mingw64/include/eigen3` est un chemin absolu Windows spécifique à une installation. Ce type de chemin hardcodé rend la build non portable.

---

## BUILD-05 🟡 Version CMake requise (3.18) et plage 3.18...3.22

**Fichier** : [CMakeLists.txt](../CMakeLists.txt)

```cmake
CMAKE_MINIMUM_REQUIRED(VERSION 3.18...3.22)
```

La syntaxe de plage `3.18...3.22` signifie que CMake 3.22 utilisera les politiques de CMake 3.22, mais CMake 3.18 utilisera les politiques de 3.18. La plage s'arrête à 3.22 alors que CMake 3.25+ est maintenant courant. Toute version > 3.22 utilisera les politiques de 3.22 (comportement de clamp).

Mettre simplement `CMAKE_MINIMUM_REQUIRED(VERSION 3.18)` ou utiliser une plage plus large (`3.18...3.30`) serait plus approprié.

---

## BUILD-06 🟡 `MARK_AS_ADVANCED` pour les chemins de sortie

**Fichier** : [CMakeLists.txt](../CMakeLists.txt)

```cmake
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
```

Ces variables legacy (`LIBRARY_OUTPUT_PATH`, `EXECUTABLE_OUTPUT_PATH`) sont dépréciées depuis CMake 2.6. Les variables modernes sont `CMAKE_LIBRARY_OUTPUT_DIRECTORY` et `CMAKE_RUNTIME_OUTPUT_DIRECTORY`.

---

## BUILD-07 🟡 `SPH_USE_GUI` : la configuration de la `sph_config.h.in` est incorrecte

**Fichier** : [sph/src/sph_config.h.in](../sph/src/sph_config.h.in)

```cmake
#cmakedefine SPH_USE_GUI "@SPH_USE_GUI@"
```

`@SPH_USE_GUI@` sera substitué par `ON` ou `OFF` (la valeur de la variable CMake), pas par `1` ou `0`. La directive `#cmakedefine VAR "value"` définit `SPH_USE_GUI` comme `"ON"` ou `"OFF"` (chaîne), ce qui est inhabituel. Normalement on utilise :
```cmake
#cmakedefine01 SPH_USE_GUI
```
qui génère `#define SPH_USE_GUI 1` ou `#define SPH_USE_GUI 0`, utilisable directement dans du code C++ sans comparaison de chaînes.

Dans le code C++, `SPH_USE_GUI` est utilisé comme :
```cpp
#ifdef SPH_USE_GUI
```
Cette directive `#ifdef` est vraie si `SPH_USE_GUI` est défini, quelle que soit sa valeur — donc même si `SPH_USE_GUI` vaut `"OFF"`, la directive est active! C'est un **bug silencieux** quand `SPH_USE_GUI=OFF`.

Cependant, la source de définition réelle de `SPH_USE_GUI` vient de la ligne :
```cmake
IF(SPH_USE_GUI)
    TARGET_SOURCES(${LIBNAME} PRIVATE ${GUI_SRCS})
    ...
ENDIF()
```
C'est là que les sources GUI sont incluses ou non. La `sph_config.h` n'est vraisemblablement pas utilisée pour ce garde, mais la confusion reste présente.

---

## BUILD-08 🟡 Absence de `CTest` / de cibles de test

Il n'existe aucune cible `test` CMake, aucune intégration `CTest`. Les tests sont lancés manuellement via `run.py`. Une intégration `CTest` permettrait `cmake --build build --target test` ou `ctest --build-config Release`.

---

## BUILD-09 🟡 La version SWIG n'est pas fixée

**Fichier** : [sph/_src/CMakeLists.txt](../sph/_src/CMakeLists.txt)

```cmake
FIND_PACKAGE(SWIG REQUIRED)
```

Aucune version minimale n'est spécifiée. SWIG 4.x introduit des changements incompatibles avec SWIG 3.x. Un changement de version non anticipé peut casser les bindings.

**Solution** :
```cmake
FIND_PACKAGE(SWIG 4.0 REQUIRED)
```
