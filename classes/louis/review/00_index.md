# Code Review — SPH Louis Goffin (C++ Rewrite)

> Review effectuée le 30 avril 2026.

Ce rapport de review est organisé en plusieurs fichiers thématiques :

| Fichier | Contenu |
|---------|---------|
| [01_overview.md](01_overview.md) | Vue d'ensemble, architecture, description |
| [02_bugs_cpp.md](02_bugs_cpp.md) | Bugs et erreurs dans le code C++ |
| [03_code_quality_cpp.md](03_code_quality_cpp.md) | Qualité de code C++ (DRY, design, couplage…) |
| [04_performance.md](04_performance.md) | Problèmes de performance |
| [05_fortran.md](05_fortran.md) | Problèmes dans le code Fortran |
| [06_python.md](06_python.md) | Problèmes dans le code Python |
| [07_build_system.md](07_build_system.md) | Système de build CMake |
| [08_improvements.md](08_improvements.md) | Propositions d'améliorations |

## Résumé exécutif

Le projet est une réécriture en C++ (avec bindings Python via SWIG) d'un code SPH original en Fortran (thèse de Louis Goffin, ULg). La qualité générale est **correcte pour un code de recherche académique**, avec une architecture bien pensée. Il reste cependant plusieurs bugs, des problèmes de conception et des opportunités d'amélioration importantes.

### Points forts
- Hiérarchie de classes propre (Particle → FixedParticle/MobileParticle)
- Interface Python bien conçue via SWIG
- GUI Qt/VTK fonctionnelle et bien découplée du moteur SPH
- Système de timers centralisé
- Parallelisme OpenMP correctement mis en œuvre dans les grandes lignes
- Gestion mémoire via `shared_ptr`
- Documentation partielle des formules physiques avec références bibliographiques

### Points faibles
- Absence totale de tests unitaires
- Bugs physiques/numériques (densité dans correction de kernel, formule viscosité)
- État global mutable (`g_timers`)
- Boucle infinie potentielle dans l'initialisateur du sorter
- Violation systématique du principe DRY (calcul d'index de cellule dupliqué)
- Couplage fort entre `Model` et les classes `Particle`
- Pas de validation des paramètres d'entrée
