# 03 — Qualité de Code C++

> Problèmes de conception, maintenabilité, lisibilité, conventions.

---

## QC-01 🔴 Violation flagrante du principe DRY — Calcul d'index de cellule dupliqué

**Fichiers** : [sph/src/sphSorter.cpp](../sph/src/sphSorter.cpp) et [sph/src/sphParticle.cpp](../sph/src/sphParticle.cpp)

Le calcul de l'index de cellule pour une position donnée est **identique** dans deux endroits distincts :

Dans `Sorter::execute()` :
```cpp
int ix = round((pos(0) - fmod(pos(0), this->dx)) / this->dx) + 1;
int iy = round((pos(1) - fmod(pos(1), this->dx)) / this->dx) + 1;
int iz = round((pos(2) - fmod(pos(2), this->dx)) / this->dx) + 1;
```

Dans `Particle::getNeighbours()` :
```cpp
int xCell = round((xyz(0) - fmod(xyz(0), sorter->dx)) / sorter->dx) + 1;
int yCell = round((xyz(1) - fmod(xyz(1), sorter->dx)) / sorter->dx) + 1;
int zCell = round((xyz(2) - fmod(xyz(2), sorter->dx)) / sorter->dx) + 1;
```

C'est exactement la même logique. Toute correction de bug ou optimisation dans l'une des copies doit être répercutée manuellement dans l'autre. Ce calcul devrait être une méthode de `Sorter` :

```cpp
// Dans Sorter :
std::array<int,3> cell_index(Eigen::Vector3d const& pos) const;
```

De plus, la formule elle-même est inutilement complexe. `round((x - fmod(x, dx)) / dx) + 1` est équivalent à `(int)(x / dx) + 1` pour `x >= 0` (avec un clamp). La version plus simple est plus lisible et légèrement plus rapide.

---

## QC-02 🔴 État global `g_timers` : non réentrant et dangereux

**Fichier** : [sph/src/sph.h](../sph/src/sph.h), [sph/src/sph.cpp](../sph/src/sph.cpp)

```cpp
extern SPH_API std::map<std::string, Timer> g_timers;
```

**Problèmes** :

1. **Non réentrant** : Il est impossible d'avoir deux instances de `Model` simultanées dans le même processus (e.g. depuis Python avec `multiprocessing.Pool` ou une interface future). Les timers se mélangeraient.

2. **Accès non thread-safe** : `g_timers["new_key"]` crée une entrée. Si deux threads créent des entrées différentes simultanément, c'est un data race sur la `std::map`. Dans le code actuel, toutes les clés sont créées avant les sections parallèles, donc ce n'est pas un problème en pratique — mais c'est une fragilité.

3. **Couplage fort** : Toutes les classes accèdent directement au global `g_timers`. Difficile à tester, difficile à modifier.

**Solution** : Faire du `Timer` un membre de `Model`, ou utiliser un `TimerSet` injecté dans les classes.

---

## QC-03 🟠 `RKstep` : état partagé via `Model` au lieu d'un paramètre

**Fichier** : [sph/src/sphModel.h](../sph/src/sphModel.h), [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
int RKstep;  // membre de Model, modifié dans solve(), lu dans toutes les particules
```

Le `RKstep` courant est stocké dans `Model` et lu par toutes les particules via `this->model->RKstep`. C'est un **état implicite partagé**. Les fonctions `update_vars()`, `getNeighbours()`, `gradW()`, `kernel_corr()` dépendent toutes de cette valeur sans la recevoir en paramètre.

Conséquences :
- Difficile à comprendre (il faut chercher où `RKstep` est modifié)
- Fragile si la boucle RK est refactorisée
- Impossible de paralléliser des étapes RK différentes

**Solution** : Passer `RKstep` comme paramètre explicite :
```cpp
virtual void update_vars(int rkstep) = 0;
void getNeighbours(int rkstep);
void gradW(int rkstep);
```

---

## QC-04 🟠 `numPart` : valeur dérivée stockée comme membre

**Fichier** : [sph/src/sphModel.h](../sph/src/sphModel.h)

```cpp
int numFP;
int numMP;
int numPart;   // ← toujours == numFP + numMP
```

`numPart` est calculé dans `add()` :
```cpp
this->numPart = this->numFP + this->numMP; // a supprimer
```

Le commentaire dit lui-même "a supprimer". Cette redondance est une source de bugs potentiels si l'invariant n'est pas maintenu. `numPart` devrait être une méthode ou supprimé au profit de `numFP + numMP` directement.

---

## QC-05 🟠 `kernelCorrection` : type `int` mais sémantique booléenne

**Fichier** : [sph/src/sphModel.h](../sph/src/sphModel.h)

```cpp
int kernelCorrection;  // 0 = off, 1 = on
```

Ce champ est utilisé comme booléen (deux valeurs possibles : `KCORR_OFF = 0` et `KCORR_ON = 1`), mais déclaré comme `int`. En Python, les tests l'assignent avec `False`/`True`. La confusion est renforcée par le fait que l'énumération `KernelCorrection` existe dans `sph.h` mais n'est pas utilisée comme type dans `Model`.

De plus, dans `update_vars()`, le `switch (this->model->kernelCorrection)` teste explicitement `KCORR_ON` et `KCORR_OFF`, ce qui serait mieux exprimé avec une véritable `enum class`.

---

## QC-06 🟠 Gravité codée en dur dans `sphMobileParticle.cpp`

**Fichier** : [sph/src/sphMobileParticle.cpp](../sph/src/sphMobileParticle.cpp)

```cpp
Eigen::Vector3d F = Eigen::Vector3d(0.0, 0.0, -9.81);  ///< Volume forces
```

La valeur de g (9.81 m/s²) et la direction de la gravité (axe Z négatif) sont **hardcodées**. Il n'est pas possible de :
- Changer g (simulations sur d'autres planètes, mise à l'échelle)
- Changer la direction de la gravité
- Désactiver la gravité (simulations sans gravité)
- Ajouter d'autres forces volumiques

Ce devrait être un paramètre de `Model`.

---

## QC-07 🟠 Constantes magiques dans `update_dt()`

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
double dt = sqrt(p->h / g);           // aucune explication de "g" local
this->timeStep = std::min(0.4 * dTf, 0.25 * dTcv);    // 0.4, 0.25 = ?
double dt = p->h / (p->c[0] + 0.6 * (this->alpha * p->c[0] + this->beta * p->max_mu_ab));  // 0.6 = ?
```

Les coefficients `0.4`, `0.25`, `0.6` sont des nombres de Courant issus de la littérature SPH (Monaghan 1989) mais aucun n'est documenté ni nommé. Ils devraient être des constantes nommées ou au moins commentées avec la référence bibliographique.

---

## QC-08 🟠 Polymorphisme et `dynamic_cast` intensif dans `to_fortran()`

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
for (auto p : this->particles)
{
    if (dynamic_cast<FixedParticle *>(p.get()))
        p->to_fortran(file_fp);
    else
        p->to_fortran(file_mp);
}

IdealGas *idealGas = dynamic_cast<IdealGas *>(this->eqState.get());
QincFluid *qincFluid = dynamic_cast<QincFluid *>(this->eqState.get());
```

L'utilisation abusive de `dynamic_cast` indique que le polymorphisme n'est pas complètement exploité. Ces opérations pourraient être remplacées par des méthodes virtuelles :
- `virtual bool is_fixed() const` dans `Particle`
- `virtual int fortran_law_code()` dans `EqState`

---

## QC-09 🟠 Couplage fort `Particle` ↔ `Model`

**Fichier** : [sph/src/sphParticle.h](../sph/src/sphParticle.h)

```cpp
Model *model;  // pointeur brut vers le Model parent
```

`Particle` accède à quasiment tout de `Model` : `model->RKstep`, `model->kernel`, `model->eqState`, `model->timeStep`, `model->alpha`, `model->beta`, `model->kernelCorrection`, `model->sorter`. C'est une **dépendance circulaire** forte qui empêche de tester `Particle` indépendamment.

---

## QC-10 🟠 `OpenMP.h` : `INTEGER` typedef incohérent entre compilateurs

**Fichier** : [sph/src/OpenMP.h](../sph/src/OpenMP.h)

```cpp
#ifdef _MSC_VER
typedef int INTEGER;
#else
typedef size_t INTEGER;
#endif
```

Ce typedef fait de `INTEGER` un type signé sur MSVC et non signé sur GCC. Du code qui l'utilise dans des comparaisons mixtes avec des `int` produit des warnings et comportements potentiellement différents entre plateformes.

---

## QC-11 🟡 Commentaires en français mélangés avec de l'anglais

Le code mélange systématiquement le français et l'anglais dans les commentaires et la documentation. Exemples :

```cpp
// [RB] me semble inutile (incr=35 et ne varie pas)   ← français
/// array of pointers toward particles.               ← anglais
// mistake in Louis' thesis: the square root is missing  ← anglais
// En ordrer to be as efficient as possible           ← "ordrer" = faute de frappe
```

Il faudrait choisir une langue (l'anglais est recommandé pour un code potentiellement public) et s'y tenir.

---

## QC-12 🟡 `QtVTKHook` : arguments `argv` en static avec `const_cast`

**Fichier** : [sph/src/gui/sphQtVTKHook.cpp](../sph/src/gui/sphQtVTKHook.cpp)

```cpp
static int argc = 1;
static std::string name = "SPH";
static char *argv[] = {const_cast<char *>(name.c_str()), nullptr};
app = new QApplication(argc, argv);
```

`const_cast` sur `name.c_str()` pour passer à `QApplication` est techniquement valide (Qt ne modifie pas `argv`), mais fragile. La variable `name` est `static` donc ne sera jamais détruite, ce qui est correct pour `argv[0]`, mais c'est un design ad-hoc. Les commentaires dans le code montrent plusieurs tentatives abandonnées ("mem leak volontaire").

---

## QC-13 🟡 La GUI lit directement les données de `Model` sans synchronisation

**Fichier** : [sph/src/gui/sphDisplayWindow.cpp](../sph/src/gui/sphDisplayWindow.cpp)

`heavy_update()` lit `model.particles[i]->rho[0]`, `model.particles[i]->coord[0]` etc. sans aucun mécanisme de synchronisation. La GUI est appelée depuis le thread principal après une barrière OpenMP implicite, donc les données sont stables quand `heavy_update()` s'exécute. Mais ce n'est pas rendu explicite par le design, et une future modification du threading pourrait introduire un data race.

---

## QC-14 🟡 Variable `pressure` réutilisée pour tous les scalaires dans la GUI

**Fichier** : [sph/src/gui/sphDisplayWindow.cpp](../sph/src/gui/sphDisplayWindow.cpp)

```cpp
double pressure = 0.0;
switch(field.code)
{
    case ScalarCode::DENSITY:    pressure = model.particles[i]->rho[0]; break;
    case ScalarCode::VELOCITY:   pressure = model.particles[i]->speed[0].norm(); break;
    // etc.
}
```

La variable est nommée `pressure` mais utilisée pour la densité, la vitesse, la masse, etc. Utiliser un nom générique comme `value` serait plus lisible.

---

## QC-15 🟡 `Model::solve()` : condition de sauvegarde fragile avec virgule flottante

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
if ((floor(this->currentTime / this->saveInt) !=
     floor((this->currentTime + this->timeStep) / this->saveInt)) ||
    ite == 0)
```

Cette condition détecte si on franchit un multiple de `saveInt`. En virgule flottante, des erreurs d'arrondi peuvent faire que la comparaison rate ou double une sauvegarde. Une approche plus robuste consiste à maintenir un compteur entier du nombre de sauvegardes effectuées :

```cpp
int next_save = (int)(currentTime / saveInt) + 1;
if (currentTime + timeStep >= next_save * saveInt || ite == 0)
    to_save = true;
```
