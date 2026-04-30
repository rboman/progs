# 08 — Propositions d'Améliorations

> Organisées par priorité et effort.

---

## 1. Corrections critiques (priorité haute, effort faible)

### 1.1 Corriger la boucle infinie dans `Sorter::init_cells()`

```cpp
// sphSorter.cpp
void Sorter::init_cells()
{
    double hmax = this->compute_hmax();
    if (hmax <= 0.0)
        throw std::runtime_error(
            "Sorter::init_cells(): hmax <= 0. "
            "Particles must be added and initialized before starting the simulation.");
    
    // Remplacer la boucle while par un calcul direct
    this->nx = std::max(1, (int)(this->model.dom_dim / (this->model.kernel->kappa * hmax)));
    // ...
}
```

### 1.2 Remplacer `vec_gradW[150]` par des vecteurs dynamiques

```cpp
// sphParticle.h
std::vector<Eigen::Vector3d> vec_gradW;
std::vector<Eigen::Vector3d> vec_gradW_mod;

// sphParticle.cpp - dans gradW()
vec_gradW.resize(this->neighbours.size());
// supprimer la vérification >150
```

### 1.3 Vérifier l'ouverture du fichier dans `save_particles()`

```cpp
file.open(filename);
if (!file.is_open())
    throw std::runtime_error("Cannot open output file: " + filename);
```

### 1.4 Corriger `sph_config.h.in` pour SPH_USE_GUI

```cmake
# sph_config.h.in
#cmakedefine01 SPH_USE_GUI
```

### 1.5 Restreindre `os.add_dll_directory()` aux répertoires connus

```python
# sph/__init__.py
if platform.system() == 'Windows' and sys.version_info.minor >= 8:
    _sph_bin_dirs = [
        os.path.join(os.path.dirname(__file__), '..', 'build', 'bin'),
        os.path.join(os.path.dirname(__file__), '..', 'build', 'bin', 'Release'),
        os.path.join(os.path.dirname(__file__), '..', 'build', 'bin', 'Debug'),
    ]
    for _d in _sph_bin_dirs:
        _d = os.path.abspath(_d)
        if os.path.isdir(_d):
            os.add_dll_directory(_d)
```

---

## 2. Améliorations de robustesse (priorité haute, effort moyen)

### 2.1 Validation des paramètres de `Model`

Ajouter une méthode `validate()` appelée en début de `run()` :

```cpp
void Model::validate() const
{
    if (this->h_0 <= 0.0)
        throw std::invalid_argument("h_0 must be > 0");
    if (this->dom_dim <= 0.0)
        throw std::invalid_argument("dom_dim must be > 0");
    if (this->numPart == 0)
        throw std::invalid_argument("No particles added to the model");
    if (this->maxTime <= 0.0)
        throw std::invalid_argument("maxTime must be > 0");
    if (this->saveInt <= 0.0)
        throw std::invalid_argument("saveInt must be > 0");
    if (!this->kernel)
        throw std::invalid_argument("kernel is not set");
    if (!this->eqState)
        throw std::invalid_argument("eqState is not set");
    // Vérifier que h_0 < kappa * dx_sorter serait idéal mais dx n'est pas encore connu
}
```

### 2.2 Passer `RKstep` en paramètre explicite

Refactorer pour éliminer l'état partagé `Model::RKstep` :

```cpp
// Signature actuelle
virtual void update_vars() = 0;

// Signature proposée
virtual void update_vars(int rkstep) = 0;
```

### 2.3 Convertir `Tee` en gestionnaire de contexte

```python
class Tee:
    def __init__(self, name):
        self._name = name

    def __enter__(self):
        self.file = open(self._name, 'w')
        self._stdout_bak = sys.stdout
        self._stderr_bak = sys.stderr
        sys.stdout = DupStream(sys.stdout, self.file)
        sys.stderr = DupStream(sys.stderr, self.file)
        return self

    def __exit__(self, *args):
        sys.stdout = self._stdout_bak
        sys.stderr = self._stderr_bak
        self.file.close()
```

### 2.4 Ajouter une protection anti-division par zéro dans `update_dt()`

```cpp
double denominator = p->c[0] + 0.6 * (this->alpha * p->c[0] + this->beta * p->max_mu_ab);
if (denominator <= 0.0)
    throw std::runtime_error("update_dt: zero speed of sound detected for particle " 
                             + std::to_string(i));
double dt = p->h / denominator;
```

---

## 3. Améliorations de design (priorité moyenne, effort important)

### 3.1 Extraire la logique d'indexation de cellule vers `Sorter`

```cpp
// Nouvelle méthode dans Sorter
std::array<int,3> Sorter::cell_index(Eigen::Vector3d const& pos) const
{
    int ix = std::clamp((int)(pos(0) / this->dx), 0, nx-1);
    int iy = std::clamp((int)(pos(1) / this->dx), 0, nx-1);
    int iz = std::clamp((int)(pos(2) / this->dx), 0, nx-1);
    return {ix, iy, iz};
}

int Sorter::flat_index(std::array<int,3> const& c) const
{
    return c[0]*nx*nx + c[1]*nx + c[2];
}
```

Utilisée à la fois dans `execute()` et dans `getNeighbours()`.

### 3.2 Déplacer `g_timers` comme membre de `Model`

```cpp
// sphModel.h
class Model {
    // ...
    std::map<std::string, Timer> timers;  // membres de l'instance
};
```

Cela permet d'avoir plusieurs instances de `Model` sans interférence, et rend le code testable.

### 3.3 Configurer la gravité dans `Model`

```cpp
// sphModel.h
Eigen::Vector3d gravity = {0.0, 0.0, -9.81};  // configurable

// sphMobileParticle.cpp
Eigen::Vector3d F = this->model->gravity;
```

### 3.4 Remplacer `int kernelCorrection` par `enum class`

```cpp
// sph.h
enum class KernelCorrection { OFF = 0, ON = 1 };

// sphModel.h
KernelCorrection kernelCorrection = KernelCorrection::OFF;
```

### 3.5 Rendre `numPart` une méthode calculée

```cpp
// sphModel.h
// Supprimer int numPart;

int numPart() const { return numFP + numMP; }
```

---

## 4. Améliorations de performance (priorité variable)

### 4.1 Ne pas re-trier à l'étape RK 1

```cpp
// sphModel.cpp, dans solve()
for (int j = 0; j < 2; j++)
{
    this->RKstep = j;
    if (j == 0)  // ← décommenter cette ligne
        this->sorter.execute();
    // ...
}
```

Gain estimé : ~10% selon le profil du code (timer "sort").

### 4.2 Paralléliser les réductions dans `update_dt()` et `update_h()`

```cpp
double dTf = std::numeric_limits<double>::max();
#pragma omp parallel for reduction(min:dTf)
for (int i = numFP; i < numPart; i++)
{
    double dt = sqrt(particles[i]->h / g);
    dTf = std::min(dTf, dt);
}
```

### 4.3 Exporter directement en VTK binaire depuis C++

Remplacer le format texte `.res` par une écriture directe en VTK (`vtkXMLUnstructuredGridWriter` déjà utilisé dans la GUI) :

```cpp
// Nouvelle méthode save_particles_vtk()
void Model::save_particles_vtk(const std::string& name, int ite, int start, int end) const
{
    vtkNew<vtkPoints> points;
    vtkNew<vtkDoubleArray> rho_arr, p_arr, ...;
    // remplir les arrays
    vtkNew<vtkUnstructuredGrid> grid;
    // ...
    vtkNew<vtkXMLUnstructuredGridWriter> writer;
    writer->SetFileName((name + "_" + id + ".vtu").c_str());
    writer->SetDataModeToBinary();
    writer->Write();
}
```

Cela éliminerait le besoin de `res2vtp.py`.

---

## 5. Infrastructure (priorité moyenne, effort faible à moyen)

### 5.1 Ajouter des tests unitaires avec un framework

Utiliser **GoogleTest** (C++) et **pytest** (Python) :

```cpp
// tests/test_kernels.cpp
TEST(CubicSplineKernel, dW_zero_at_kappa)
{
    CubicSplineKernel k;
    double h = 0.1;
    EXPECT_NEAR(k.dW(2.0 * h, h), 0.0, 1e-10);  // dW = 0 pour r = kappa*h
}

TEST(QincFluid, pressure_at_rho0_is_zero)
{
    QincFluid f;
    EXPECT_NEAR(f.pressure(f.rho0), 0.0, 1e-10);
}
```

### 5.2 Intégrer CTest dans CMake

```cmake
# CMakeLists.txt
INCLUDE(CTest)
ENABLE_TESTING()
ADD_SUBDIRECTORY(tests)

# tests/CMakeLists.txt
ADD_EXECUTABLE(test_kernels test_kernels.cpp)
TARGET_LINK_LIBRARIES(test_kernels sph GTest::GTest GTest::Main)
ADD_TEST(NAME test_kernels COMMAND test_kernels)
```

### 5.3 Ajouter un `.clang-format` pour uniformiser le style

```yaml
# .clang-format
BasedOnStyle: LLVM
IndentWidth: 4
BreakBeforeBraces: Allman
```

### 5.4 Ajouter un `requirements.txt` ou `pyproject.toml` Python

```
# requirements.txt
vtk>=9.0
numpy
```

### 5.5 Documenter les invariants de classe dans les headers

Ajouter des contrats explicites :

```cpp
class Model {
    // Invariants:
    //   - particles[0..numFP-1] are FixedParticle instances
    //   - particles[numFP..numPart-1] are MobileParticle instances
    //   - numPart == numFP + numMP (always)
    //   - h_0 > 0 when run() is called
    //   - dom_dim > 0 when run() is called
};
```

---

## 6. Refactorings physiques / numériques

### 6.1 Documenter et corriger l'incohérence KCORR_ON pour `drho_dt`

```cpp
// Ajouter un commentaire explicatif ou corriger :
case KCORR_ON:
    // Note: density equation uses uncorrected gradW (intentional, see Randles & Libersky 1996)
    //       momentum equation uses corrected gradW_mod
    drho_dt += this->m * u_ab.dot(this->vec_gradW[i]);      // ← intentionnel ?
    du_dt   += this->m * (...) * this->vec_gradW_mod[i];
```

### 6.2 Nommer les constantes de la condition CFL

```cpp
// sphModel.cpp
static constexpr double CFL_BODY_FORCE  = 0.4;
static constexpr double CFL_COURANT_VIS = 0.25;
static constexpr double VISCOSITY_COEFF = 0.6;

this->timeStep = std::min(CFL_BODY_FORCE * dTf, CFL_COURANT_VIS * dTcv);
```

### 6.3 Rendre la direction et la valeur de g configurables

```cpp
model.gravity = Eigen::Vector3d(0, 0, -9.81);   // default : Earth
// ou model.gravity = Eigen::Vector3d::Zero();   // apesanteur
```

### 6.4 Ajouter une re-initialisation du sorter si `h` change significativement

```cpp
void Model::update_h()
{
    // ...
    double new_h = ...;
    
    // Forcer la ré-initialisation du sorter si h change de plus de 10%
    if (sorter.dx > 0.0 && std::abs(new_h - this->h_0) / this->h_0 > 0.1)
        sorter.reset();  // force un prochain init_cells()
    // ...
}
```

---

## Tableau récapitulatif

| # | Description | Priorité | Effort | Impact |
|---|-------------|----------|--------|--------|
| 1.1 | Boucle infinie Sorter | 🔴 Critique | Faible | Stabilité |
| 1.2 | vec_gradW dynamique | 🔴 Critique | Faible | Stabilité |
| 1.3 | Vérif. ouverture fichier | 🟠 Haute | Très faible | Robustesse |
| 1.5 | DLL directory sécurisé | 🔴 Critique | Faible | Sécurité |
| 2.1 | Validation paramètres | 🟠 Haute | Moyen | Robustesse |
| 2.2 | RKstep en paramètre | 🟡 Moyenne | Important | Design |
| 3.1 | DRY cell indexing | 🟠 Haute | Faible | Maintenabilité |
| 3.2 | g_timers → Model | 🟡 Moyenne | Moyen | Design |
| 3.3 | Gravité configurable | 🟡 Moyenne | Faible | Fonctionnalité |
| 4.1 | Éviter re-tri RK2 | 🟠 Haute | Très faible | Perf +10% |
| 4.3 | Export VTK direct | 🟡 Moyenne | Important | Perf IO |
| 5.1 | Tests unitaires | 🟠 Haute | Important | Qualité |
| 5.3 | clang-format | 🟡 Basse | Très faible | Style |
