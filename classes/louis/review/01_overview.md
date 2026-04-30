# 01 — Vue d'ensemble et Architecture

## Description du projet

Le projet est une réécriture en C++17 du code SPH (*Smoothed Particle Hydrodynamics*) développé dans la thèse de Louis Goffin (Université de Liège, référence ORBi 2268/156166). Le code simule des fluides en utilisant la méthode SPH, une approche Lagrangienne sans maillage.

### Objectif physique

La méthode SPH discrétise les équations de Navier-Stokes sous forme Lagrangienne :
- **Équation de continuité** (mise à jour de la densité ρ)
- **Équation de quantité de mouvement** (mise à jour de la vitesse u)
- **Équation d'état** (loi pression–densité : gaz parfait ou fluide quasi-incompressible)
- **Intégration temporelle** : schéma de Runge-Kutta d'ordre 2 (RK22 / méthode de Heun)

### Fonctionnalités

- Deux implémentations parallèles : Fortran (originale) et C++ (réécriture)
- Interface Python via SWIG (bindings auto-générés)
- GUI Qt 5/6 + VTK 9 pour la visualisation temps-réel
- Export des résultats au format VTK (`.vtu`, `.vts`) pour Paraview
- Support OpenMP pour le calcul parallèle
- Multi-plateforme : Windows (MSVC, MinGW, MSYS2), Linux, HPC (Nic5)

---

## Architecture générale

```
tests/*.py          ← scripts de test (scènes SPH Python)
run.py              ← lanceur : configure l'env, crée le workdir, execute le test

sph/                ← module Python "sph"
  __init__.py       ← importe _sphw et utils
  utils.py          ← setupwdir, parseargs, Tee (tee stdout)
  helpers.py        ← Runner, Box, Sphere, filled/hollow
  res2vtp.py        ← conversion .res → .vtu (VTK)
  _src/
    sphw.i          ← interface SWIG → génère _sphw.pyd
    wCppBuf2Py.{h,cpp} ← redirect std::cout → Python sys.stdout
  src/
    sph.{h,cpp}         ← namespace sph, g_timers, print_banner
    sphModel.{h,cpp}    ← classe Model (chef d'orchestre)
    sphParticle.{h,cpp} ← classe Particle (base)
    sphFixedParticle.{h,cpp} ← particule fixe (frontière)
    sphMobileParticle.{h,cpp} ← particule mobile (fluide)
    sphSorter.{h,cpp}   ← tri par listes chaînées (grille)
    sphKernels.{h,cpp}  ← kernels SPH (cubic spline, quadratic, quintic)
    sphEqState.h        ← équations d'état (IdealGas, QincFluid)
    sphTimer.{h,cpp}    ← timer haute résolution
    sphNeighbour.h      ← paire (Particle*, distance)
    OpenMP.h            ← wrapper OpenMP portable
    gui/
      sphDisplayHook.h       ← interface abstraite (pattern Strategy)
      sphQtVTKHook.{h,cpp}   ← implémentation Qt : crée QApplication + window
      sphDisplayWindow.{h,cpp} ← widget VTK (affichage particules, UI)
      DisplayWindow.ui       ← layout Qt Designer

src_f/              ← code Fortran original
  SPH_module.f90    ← module principal Fortran
  SPH_simulation.f90 ← programme principal Fortran
  mod_time.f90      ← timers Fortran
```

---

## Flux d'exécution

### Mode C++ (`--cpp`)
```
run.py → setupwdir() → import sph → parseargs()
  → tests/waterdrop.py::model()
    → Box.generate() → model.add(MobileParticle / FixedParticle)
    → Runner.run_cpp()
      → [si GUI] QtVTKHook(model) → model.set_hook(hook)
      → model.run()
        → initialise() toutes les particules
        → solve()  ← boucle temporelle
          → pour chaque pas de temps :
            → [RK step 0 et 1] sorter.execute() + parallel update_vars()
            → copy_vars() : [2] → [0]
            → [si save] save_particles() + displayHook.update_data()
            → displayHook.interact()  ← Qt processEvents
            → update_dt() + update_h()
  → res2vtp.ToParaview().convertall()  ← .res → .vtu
```

### Mode Fortran (défaut)
```
run.py → model.to_fortran()  ← écrit input.mp, input.fp, input.prm
  → subprocess louis.exe  ← lance le binaire Fortran
  → res2vtp.ToParaview().convertall()
```

---

## Points forts de l'architecture

| Point fort | Détail |
|-----------|--------|
| **Découplage GUI** | `DisplayHook` est une interface abstraite pure. Le moteur SPH n'a aucune dépendance Qt/VTK directe. |
| **Polymorphisme particle** | `Particle → FixedParticle / MobileParticle` avec `update_vars()` virtuelle. Extensible. |
| **Kernels polymorphes** | `Kernel → CubicSplineKernel / QuadraticKernel / QuinticSplineKernel`. Facile d'en ajouter. |
| **EqState polymorphe** | `EqState → IdealGas / QincFluid`. Facile d'en ajouter. |
| **Gestion mémoire** | Particules gérées par `shared_ptr`. Pas de `new/delete` exposés. |
| **Interface Python** | Via SWIG avec gestion des exceptions C++ → Python. |
| **Timers intégrés** | Timers centralisés par nom (`g_timers["sort"]`, etc.) facilitant le profiling. |

---

## Faiblesses architecturales majeures

| Faiblesse | Détail |
|-----------|--------|
| **Couplage circulaire** | `Particle` connaît `Model` (via `model*`), `Model` connaît `Particle`. Couplage fort. |
| **État global** | `g_timers` est une variable globale exportée de la DLL. Non réentrant. |
| **Deux implémentations** | Le code Fortran et le C++ sont maintenus en parallèle avec un pont `to_fortran()`. Coût de maintenance élevé. |
| **Pas de tests unitaires** | Seuls des tests d'intégration "full simulation" existent (scripts Python). |
| **Paramètres non validés** | `Model` n'effectue aucune validation de ses paramètres avant `run()`. |
| **`numPart` redondant** | `numPart = numFP + numMP` est maintenu manuellement → risque d'inconsistance. |

---

## Dépendances externes

| Dépendance | Usage | Obligatoire |
|-----------|-------|-------------|
| CMake ≥ 3.18 | Build system | Oui |
| C++17 | Langage | Oui |
| Eigen3 | Algèbre linéaire (Vector3d) | Oui |
| Python3 | Scripting + interface | Oui |
| SWIG | Génération des bindings | Oui |
| OpenMP | Parallélisme | Optionnel |
| Qt5 ou Qt6 | GUI | Optionnel (`SPH_USE_GUI`) |
| VTK 9 | Rendu 3D + export | Optionnel (requis si GUI) |
| gfortran / ifort | Code Fortran | Optionnel |
