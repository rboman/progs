# 02 — Bugs et Erreurs — Code C++

> Sévérité : 🔴 Critique | 🟠 Important | 🟡 Mineur

---

## BUG-01 🔴 Boucle infinie dans `Sorter::init_cells()` si `hmax = 0`

**Fichier** : [sph/src/sphSorter.cpp](../sph/src/sphSorter.cpp)

```cpp
void Sorter::init_cells()
{
    double hmax = this->compute_hmax();
    this->nx = 0;
    while (this->model.dom_dim / (this->nx + 1) > this->model.kernel->kappa * hmax)
        this->nx++;
    // ...
}
```

**Problème** : Si `hmax == 0` (ce qui arrive si aucune particule n'a été ajoutée avant l'initialisation, ou si `h` vaut 0), le membre droit `kappa * hmax` vaut 0, et la condition `dom_dim / (nx+1) > 0` reste vraie indéfiniment pour tout `dom_dim > 0`. **Le programme boucle à l'infini.**

`compute_hmax()` retourne 0 si toutes les particules ont `h = 0`. Les particules sont initialisées dans `Model::run()` → `initialise()`, mais `Sorter::execute()` / `init_cells()` est appelé depuis `solve()` donc après `initialise()`. Cependant, si `h_0` n'est pas configuré, toutes les particules auront `h = 0` avant l'initialisation.

De plus, si l'utilisateur appelle `solve()` directement sans passer par `run()`, il peut déclencher ce bug.

**Correction suggérée** :
```cpp
void Sorter::init_cells()
{
    double hmax = this->compute_hmax();
    if (hmax <= 0.0)
        throw std::runtime_error("Sorter::init_cells: hmax <= 0 (particles not initialized?)");
    // ...
}
```

---

## BUG-02 🔴 Tableau fixe `vec_gradW[150]` : dépassement silencieux

**Fichier** : [sph/src/sphParticle.h](../sph/src/sphParticle.h), [sphParticle.cpp](../sph/src/sphParticle.cpp)

```cpp
Eigen::Vector3d vec_gradW[150];
Eigen::Vector3d vec_gradW_mod[150];
```

```cpp
void Particle::gradW()
{
    if (this->neighbours.size() > 150)
        throw std::runtime_error("number of neighbours greater than expected (max 150 for vec_gradW)...");
    // ...
}
```

**Problème** : La limite de 150 voisins est une constante **arbitraire et non configurable**. Pour des simulations denses ou avec un grand `kappa` (e.g. QuinticSplineKernel avec `kappa=3`), ce seuil peut facilement être dépassé, interrompant la simulation brutalement sans possibilité de récupération.

De plus, `vec_gradW_mod` est utilisé dans `kernel_corr()` mais ce tableau n'est pas vérifié dans cette fonction — seul `gradW()` effectue la vérification. Si `kernel_corr()` est appelée avec plus de 150 voisins, c'est un **accès mémoire hors-limites** (undefined behavior).

**Correction suggérée** : Utiliser `std::vector<Eigen::Vector3d>` alloué dynamiquement :
```cpp
std::vector<Eigen::Vector3d> vec_gradW;
std::vector<Eigen::Vector3d> vec_gradW_mod;
```
Et redimensionner dans `gradW()` :
```cpp
vec_gradW.resize(this->neighbours.size());
```

---

## BUG-03 🔴 `to_fortran()` : écriture de valeurs par défaut au lieu des valeurs utilisateur

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
if (idealGas)
{
    file_prm << LAW_IDEAL_GAS << '\n';
    file_prm << QincFluid().gamma << '\n';  // ← BUG : crée une QincFluid TEMPORAIRE avec gamma par défaut (7.0) !
    file_prm << idealGas->M << '\n';
}
else if(qincFluid)
{
    file_prm << LAW_QINC_FLUID << '\n';
    file_prm << qincFluid->gamma << '\n';
    file_prm << IdealGas().M << '\n';        // ← BUG : crée un IdealGas TEMPORAIRE avec M par défaut !
}
```

**Problème** : Lorsque l'équation d'état est `IdealGas`, le code écrit le `gamma` d'une `QincFluid` temporaire créée avec ses valeurs **par défaut** (7.0), quel que soit le `QincFluid` éventuellement configuré. De même pour l'autre branche.

Ce code est conçu pour écrire une "dummy value" que le code Fortran n'utilisera pas pour le type concerné. C'est une approche fragile et silencieusement incorrecte si le code Fortran change. Il n'y a aucun commentaire expliquant ce choix.

**Correction suggérée** : Documenter clairement l'intention, ou mieux, utiliser des constantes explicites :
```cpp
file_prm << QincFluid::default_gamma << '\n';  // valeur factice, non utilisée par Fortran pour IdealGas
```

---

## BUG-04 🟠 `Sorter::init_cells()` appelée une seule fois : grille jamais recalculée

**Fichier** : [sph/src/sphSorter.cpp](../sph/src/sphSorter.cpp)

```cpp
void Sorter::execute()
{
    if (this->cells.size() == 0)
        this->init_cells();   // ← appelée UNE seule fois
    // ...
}
```

**Problème** : La taille des cellules (`dx`) est calculée en fonction de `hmax` lors de la **première** itération. Comme `update_h()` peut faire varier le smoothing length `h` au cours de la simulation, la grille peut devenir inadaptée :
- Si `h` augmente, les cellules peuvent devenir trop petites → recherche de voisins incomplète (faux négatifs).
- Si `h` diminue, les cellules sont trop grandes → perte d'efficacité.

Dans le cas où `h` est stable (ce qui est l'usage typique), ce n'est pas un bug. Mais si `update_h()` modifie significativement `h`, la simulation peut produire des résultats incorrects **silencieusement**.

---

## BUG-05 🟠 Division par zéro dans `update_dt()` si vitesse du son nulle

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
double dt = p->h / (p->c[0] + 0.6 * (this->alpha * p->c[0] + this->beta * p->max_mu_ab));
```

**Problème** : Si `p->c[0] == 0` et `alpha == beta == 0`, le dénominateur est zéro. `h / 0` produit `inf`, `min(inf, inf)` = `inf`, et le timestep devient infini. La simulation va alors avancer d'une infinité de temps en un seul pas.

Bien que les équations d'état par défaut (QincFluid, IdealGas) retournent des vitesses du son non nulles avec des paramètres raisonnables, rien n'empêche un utilisateur de configurer `c0 = 0`.

---

## BUG-06 🟠 Inconsistance dans `update_vars()` avec correction de kernel (KCORR_ON)

**Fichier** : [sph/src/sphMobileParticle.cpp](../sph/src/sphMobileParticle.cpp)

```cpp
case KCORR_ON:
{
    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
        drho_dt += this->m * u_ab.dot(this->vec_gradW[i]);      // ← utilise le gradient NON corrigé
        du_dt   += this->m * (...) * this->vec_gradW_mod[i];    // ← utilise le gradient CORRIGÉ
    }
}
```

**Problème** : Quand la correction de kernel est activée (`KCORR_ON`), l'équation de continuité (pour `drho_dt`) utilise le gradient **non corrigé** `vec_gradW[i]`, alors que l'équation de quantité de mouvement (pour `du_dt`) utilise le gradient **corrigé** `vec_gradW_mod[i]`. Cette inconsistance peut-être intentionnelle (certains schémas SPH ne corrigent que le moment), mais elle n'est **pas documentée** et représente un écart avec la formulation présentée dans la thèse.

---

## BUG-07 🟠 `Model::add()` : hypothèse sur l'ordre des particules non garantie

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
std::shared_ptr<Particle>
Model::add(std::shared_ptr<Particle> p)
{
    if (dynamic_cast<FixedParticle *>(p.get()))
    {
        // TODO: attention, le code fait parfois l'hypothèse que les fixes précèdent les mobiles!
        if (this->numMP != 0)
            throw std::runtime_error("Fixed particles must be added before mobile particles");
        this->numFP++;
    }
    // ...
}
```

Le `throw` protège effectivement l'ordre. **Mais** le commentaire TODO indique que des parties du code font quand même l'hypothèse que les particules fixes occupent les indices `[0, numFP-1]` et les mobiles `[numFP, numPart-1]`. Cette invariante n'est pas documentée dans la classe `Model`, et serait brisée si quelqu'un modifie `add()` sans en tenir compte.

En particulier, `save_particles()`, `addParticles()` dans le GUI, et `updateParticlePositions()` font tous cette hypothèse implicitement.

---

## BUG-08 🟠 `save_particles()` : absence de vérification d'ouverture du fichier

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
void Model::save_particles(std::string const &name, int ite, int start, int end) const
{
    std::ofstream file;
    file.open(filename);
    for (int i = start; i <= end; ++i)
        this->particles[i]->save(file);
    file.close();
}
```

**Problème** : Si `file.open()` échoue (disque plein, permissions, etc.), `file.is_open()` retourne `false` mais aucune exception n'est levée. Les `file << ...` sont ignorés silencieusement, et la simulation continue sans sauvegarder, sans avertissement.

**Correction** :
```cpp
if (!file.is_open())
    throw std::runtime_error("Cannot open file: " + filename);
```

---

## BUG-09 🟠 `wCppBuf2Py.cpp` : `xsputn` retourne la taille tronquée

**Fichier** : [sph/_src/wCppBuf2Py.cpp](../sph/_src/wCppBuf2Py.cpp)

```cpp
std::streamsize CppBuf2Py::xsputn(const char *s, std::streamsize n)
{
    static const std::streamsize MAXSIZE = 1000;
    std::streamsize written = std::min(n, MAXSIZE);
    // ...
    return written;  // ← retourne min(n, 1000), pas n
}
```

**Problème** : Le contrat de `xsputn` est de retourner le **nombre de caractères effectivement écrits**. En retournant `min(n, 1000)` au lieu de `n`, si `n > 1000`, le stream C++ croit que seulement 1000 caractères ont été consommés et peut émettre une erreur ou se mettre en état `fail`. En pratique `PySys_WriteStdout` transmet la chaîne complète (la troncature concerne l'affichage Python, pas la transmission). Cependant, retourner `n` (la valeur correcte) serait plus robuste.

---

## BUG-10 🟡 `Model::Model()` : type mismatch dans l'initialisation

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
this->kernelCorrection = 0.0;  // kernelCorrection est un int, mais assigné avec un double
```

Compilation OK (conversion implicite), mais inconsistant. Devrait être `0`.

---

## BUG-11 🟡 `Particle::model` : pointeur nu vers le `Model` propriétaire

**Fichier** : [sph/src/sphParticle.h](../sph/src/sphParticle.h)

```cpp
Model *model;  // raw pointer
```

Les particules sont détenues via `shared_ptr<Particle>` dans le `Model`. Si une particule est conservée en vie après la destruction du `Model` (scénario possible depuis Python), `model` devient un **pointeur dangling**. Tout accès ultérieur depuis la particule est un comportement indéfini.

---

## BUG-12 🟡 `cellsToCheck` non initialisé dans `getNeighbours()`

**Fichier** : [sph/src/sphParticle.cpp](../sph/src/sphParticle.cpp)

```cpp
int cellsToCheck[27];
// ...
for (int i = -1; i < 2; i++)
    for (int j = -1; j < 2; j++)
        for (int k = -1; k < 2; k++)
        {
            if (...bounds check...)
                cellsToCheck[(i+1)*9 + (j+1)*3 + (k+2)-1] = ...;
            else
                cellsToCheck[(i+1)*9 + (j+1)*3 + (k+2)-1] = -1;
        }
```

Le tableau est correctement rempli pour les 27 positions (l'indexation couvre bien [0..26]). Pas de bug fonctionnel, mais le tableau local n'est pas initialisé avant la boucle, ce qui génère des warnings dans certains compilateurs. Une initialisation `= {-1}` initiale serait plus défensive.

---

## BUG-13 🟡 `update_h()` : la limite du smoothing length utilise `sorter.dx` avant initialisation

**Fichier** : [sph/src/sphModel.cpp](../sph/src/sphModel.cpp)

```cpp
if (new_h > 0.5 * this->sorter.dx)
{
    new_h = 0.5 * this->sorter.dx;
    std::cout << "Warning: the smoothing has been limited" << std::endl;
}
```

`sorter.dx` vaut 0.0 jusqu'au premier appel de `init_cells()` (qui se produit au premier `execute()`). Si `update_h()` était appelée avant le premier `solve()`, cette limite serait 0, réduisant `h` à 0 et provoquant des divisions par zéro en cascade. Dans le flux actuel, ce n'est pas le cas, mais la fragilité demeure.
