# 04 — Performance

> Goulots d'étranglement, optimisations manquées, scalabilité.

---

## PERF-01 🔴 Sorter appelé deux fois par pas de temps (une fois par étape RK)

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
for (int j = 0; j < 2; j++)
{
    this->RKstep = j;
    // if(j==0)      ← commenté !
    this->sorter.execute();   // tri pour les deux étapes RK
    // ...
}
```

Le commentaire `// if(j==0)` montre que l'auteur a envisagé de ne trier qu'une seule fois par pas de temps. La justification de trier deux fois est que les particules se déplacent entre l'étape 0 et l'étape 1. Mais le commentaire dans `getNeighbours()` indique explicitement :

> "For the second RK step, only the distances r are recalculated. It is assumed that the neighbours remain the same between 2 RK steps."

Donc les **voisins** sont réutilisés mais le **sorter** est quand même réexécuté à l'étape 1. Ce re-tri est redondant puisque les voisins ne changent pas entre les deux étapes RK. Le sorter de l'étape 1 sert uniquement à re-placer les particules dans les cellules avec leurs positions mises à jour `coord[1]`, mais ces positions mises à jour ne sont utilisées dans `getNeighbours()` qu'en lecture (calcul de distance `r`), sans re-cherche de voisins.

**Impact** : Le tri représente probablement ~10-20% du temps de calcul (`g_timers["sort"]`). L'éviter pour l'étape 1 serait un gain direct de ~10%.

---

## PERF-02 🔴 Tableaux `vec_gradW[150]` : faux partage mémoire (false sharing)

**Fichier** : [sph/src/sphParticle.h](../sph/src/sphParticle.h)

```cpp
Eigen::Vector3d vec_gradW[150];      // 150 * 3 * 8 = 3600 bytes
Eigen::Vector3d vec_gradW_mod[150];  // 3600 bytes
```

Chaque particule alloue systématiquement 7.2 Ko pour ces deux tableaux, indépendamment du nombre réel de voisins (souvent 20-60 en pratique). Pour 10 000 particules, cela représente **72 Mo** de mémoire inutilisée.

De plus, avec OpenMP, si deux particules sont sur la même ligne de cache (64 octets), leur accès concurrent peut provoquer du **false sharing**, dégradant les performances. Un `std::vector<Eigen::Vector3d>` alloué dynamiquement permettrait de mieux contrôler la localité mémoire.

---

## PERF-03 🟠 `update_h()` et `update_dt()` : boucles séquentielles sur toutes les particules

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
void Model::update_h()
{
    double mean_rho = 0.0;
    for (int i = 0; i < this->numPart; i++)  // séquentiel
        mean_rho += this->particles[i]->rho[0];
    // ...
}

void Model::update_dt()
{
    double dTf = std::numeric_limits<double>::max();
    for (int i = this->numFP; i < this->numPart; i++)  // séquentiel
    {
        // ...
        if (dt < dTf) dTf = dt;
    }
    // ...
}
```

Ces deux fonctions effectuent des réductions (somme, minimum) sur l'ensemble des particules. Elles sont appelées séquentiellement alors qu'elles sont facilement parallélisables via des réductions OpenMP :

```cpp
#pragma omp parallel for reduction(+:mean_rho)
for (int i = 0; i < this->numPart; i++)
    mean_rho += this->particles[i]->rho[0];
```

Le commentaire dans `compute_hmax()` le note lui-même : `// TODO: reduction openmp??`

---

## PERF-04 🟠 `g_timers` : accès par clé `std::string` à chaque timer

**Fichier** : [sph/src/sph.h](../sph/src/sph.h)

```cpp
g_timers["sort"].start();
// ...
g_timers["sort"].stop();
```

Chaque accès à `g_timers["sort"]` effectue un lookup dans une `std::map<std::string, Timer>` (O(log n) avec comparaison de chaînes). Ces appels se produisent à chaque itération de la boucle temporelle pour plusieurs timers. Sur des simulations longues avec des millions d'itérations, le coût cumulé est non nul.

**Solution** : Utiliser des références aux timers une seule fois au début de la simulation, ou utiliser des `Timer` directement comme membres de `Model`.

---

## PERF-05 🟠 Voisins stockés dans un `std::vector` avec `push_back`

**Fichier** : [sph/src/sphParticle.cpp](../sph/src/sphParticle.cpp)

```cpp
this->neighbours.clear();
// ...
this->neighbours.push_back(Neighbour(p, r));
```

`std::vector::push_back` peut entraîner des réallocations répétées. À l'étape 0 de chaque pas de temps, la liste des voisins est complètement reconstruite. Si `reserve()` est appelé avec le nombre de voisins de l'itération précédente, les réallocations seraient évitées :

```cpp
size_t prev_size = this->neighbours.size();
this->neighbours.clear();
this->neighbours.reserve(prev_size);  // evite les reallocations si le nb de voisins est stable
```

---

## PERF-06 🟠 Sélection de champ scalaire dans la GUI avec un `switch` O(n) répété

**Fichier** : [sph/src/gui/sphDisplayWindow.cpp](../sph/src/gui/sphDisplayWindow.cpp)

Le même `switch(field.code)` avec 8 cas est répété **deux fois** dans `updateParticlePositions()` (une fois pour les particules fixes, une fois pour les mobiles). Ces boucles itèrent sur potentiellement des milliers de particules. Le `switch` pourrait être factorisé en dehors de la boucle avec un lambda ou un pointeur de fonction :

```cpp
auto getter = [&](Particle* p) -> double {
    switch(field.code) { ... }
};
for (int i = 0; i < model.numFP; i++)
    fixed_scalars->SetTuple1(i, getter(model.particles[i].get()));
```

---

## PERF-07 🟠 `res2vtp.py` : format texte `.res` lent à lire et écrire

**Fichier** : [sph/res2vtp.py](../sph/res2vtp.py) et [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

Les résultats sont sauvegardés en **format texte ASCII** (`.res`), puis convertis en VTK binaire (`.vtu`). Pour de grandes simulations :
- La conversion textuelle est lente (parsing de chaque nombre en virgule flottante)
- Les fichiers `.res` sont volumineux (facteur ~3-5 vs binaire)
- La conversion en parallèle (`multiprocessing.Pool`) compense partiellement, mais ne résout pas le problème à la source

Un export direct en VTK binaire depuis le C++ serait bien plus efficace (le README le mentionne comme TODO : "sauvegarde direct en `.vtp`").

---

## PERF-08 🟡 `Sorter::init_cells()` : `while` loop pour calculer `nx`

**Fichier** : [sph/src/sphSorter.cpp](../sph/src/sphSorter.cpp)

```cpp
this->nx = 0;
while (this->model.dom_dim / (this->nx + 1) > this->model.kernel->kappa * hmax)
    this->nx++;
```

Le résultat est simplement `floor(dom_dim / (kappa * hmax))`. La boucle est O(nx) = O(dom_dim / (kappa * hmax)) et inutile :

```cpp
this->nx = (int)(this->model.dom_dim / (this->model.kernel->kappa * hmax));
if (this->nx < 1) this->nx = 1;
```

---

## PERF-09 🟡 Allocation/désallocation des listes de voisins à chaque `getNeighbours()`

Le `neighbours.clear()` n'est appelé qu'à l'étape 0 et les capacités du vector sont conservées entre itérations. C'est un bon point. Mais les objets `Neighbour` (pointeur + double) sont copiés par valeur dans `push_back`. Pour un vecteur pré-alloué, ceci est acceptable.

---

## Résumé des optimisations prioritaires

| # | Gain estimé | Action |
|---|------------|--------|
| PERF-01 | ~10% | Ne pas re-trier à l'étape RK 1 |
| PERF-03 | ~5% | OpenMP pour `update_h`, `update_dt` |
| PERF-07 | IO x5 | Export binaire direct (VTK natif ou HDF5) |
| PERF-02 | Mémoire | `vec_gradW` en vector dynamique |
| PERF-05 | Allocation | `reserve()` pour le vecteur de voisins |
