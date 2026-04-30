# 06 — Code Python

> Review de la couche Python : sph/__init__.py, sph/utils.py, sph/helpers.py, sph/res2vtp.py, run.py, tests/*.py

---

## PY-01 🔴 `sph/__init__.py` : ajout de tous les répertoires du PATH comme répertoires DLL

**Fichier** : [sph/__init__.py](../sph/__init__.py)

```python
for v in os.environ['path'].split(';'):
    if os.path.exists(v):
        os.add_dll_directory(v)
```

**Problème de sécurité** : Cette boucle ajoute **chaque répertoire présent dans PATH** comme répertoire de recherche DLL pour Windows. Si un attaquant peut placer une DLL malveillante dans n'importe quel répertoire du PATH (scénario de DLL hijacking), elle sera chargée. C'est un problème bien documenté sur Windows.

**Solution** : Ajouter uniquement les répertoires des binaires connus du projet :
```python
if platform.system() == 'Windows' and sys.version_info.minor >= 8:
    # Add only the binary directory of the project
    sph_dir = os.path.dirname(os.path.abspath(__file__))
    bin_dirs = [
        os.path.join(sph_dir, '..', 'build', 'bin'),
        os.path.join(sph_dir, '..', 'build', 'bin', 'Release'),
    ]
    for d in bin_dirs:
        d = os.path.abspath(d)
        if os.path.isdir(d):
            os.add_dll_directory(d)
```

---

## PY-02 🟠 `run.py` : `exec(compile(script, ...))` pour exécuter des tests

**Fichier** : [run.py](../run.py)

```python
def run_simulation(testname):
    script = open(testname, encoding='utf-8').read()
    exec(compile(script, testname, 'exec'),
         {'__file__': testname, '__name__': '__main__'})
```

**Problèmes** :

1. **Sécurité** : `exec()` exécute du code Python arbitraire. Bien que dans un contexte académique le risque soit limité, un script de test malveillant ou mal formé peut faire n'importe quoi dans le processus courant.

2. **Isolation** : Les variables locales de `run_simulation()` sont exposées via le contexte `exec`. Le script de test s'exécute dans un espace de noms partiel, ce qui peut causer des comportements inattendus si des variables sont accidentellement partagées.

3. **Pas de timeout** : Si un test boucle indéfiniment, le process est bloqué.

**Solution préférable** : Importer le module dynamiquement via `importlib` :
```python
import importlib.util
spec = importlib.util.spec_from_file_location("test_module", testname)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)
```

---

## PY-03 🟠 `parseargs()` appelé plusieurs fois de manière répétée

**Fichier** : [sph/utils.py](../sph/utils.py), [sph/helpers.py](../sph/helpers.py)

```python
# Dans Runner.run():
args = sph.parseargs()

# Dans Runner.run_cpp():
args = sph.parseargs()

# Dans Runner.run_fortran():
args = sph.parseargs()

# Dans run.py:
args = sph.parseargs()   # plusieurs fois
```

`parseargs()` appelle `argparse.ArgumentParser().parse_args()` à chaque invocation, ce qui re-parse `sys.argv` à chaque appel. C'est inefficace et, pire, si `sys.argv` était modifié entre deux appels, les résultats seraient incohérents.

**Solution** : Appeler `parseargs()` une seule fois et passer l'objet `args` en paramètre dans toute la chaîne d'appels, ou utiliser une variable module-level.

---

## PY-04 🟠 `Tee` utilise `__del__` pour la restauration des streams

**Fichier** : [sph/utils.py](../sph/utils.py)

```python
class Tee:
    def __init__(self, name):
        self.file = open(name, 'w')
        self.stdoutbak = sys.stdout
        self.stderrbak = sys.stderr
        sys.stdout = DupStream(sys.stdout, self.file)
        sys.stderr = DupStream(sys.stderr, self.file)

    def __del__(self):
        sys.stdout = self.stdoutbak
        sys.stderr = self.stderrbak
        self.file.close()
```

**Problèmes** :

1. **`__del__` non garanti** : Python ne garantit pas l'ordre de destruction des objets. Si une exception survient dans `__init__` après l'assignation de `sys.stdout`, le `__del__` peut ne jamais être appelé, laissant le stream redirigé indéfiniment.

2. **`__del__` appelé à la mauvaise heure** : Si le garbage collector décide de détruire `tee` pendant qu'une exception remonte, les logs d'erreurs risquent d'être perdus.

**Solution** : Implémenter `Tee` comme gestionnaire de contexte :
```python
class Tee:
    def __enter__(self):
        ...
        return self

    def __exit__(self, *args):
        sys.stdout = self.stdoutbak
        sys.stderr = self.stderrbak
        self.file.close()

# Usage:
with Tee('stdout.txt'):
    run_simulation(testname)
```

---

## PY-05 🟠 `res2vtp.py` : `except:` attrape tout, y compris `KeyboardInterrupt`

**Fichier** : [sph/res2vtp.py](../sph/res2vtp.py)

```python
try:
    x, y, z, vx, vy, vz, rho, p, m, c, h, mu, nv = list(map(float, line.strip().split()))
except:
    print("**ERROR while reading file %s!\n\tline=\"%s\"" % (fname, line))
    break
```

Un `except:` sans type de l'exception attrape **tout**, y compris `SystemExit`, `KeyboardInterrupt`, et `MemoryError`. Si l'utilisateur fait Ctrl+C pendant la conversion, l'exception est ignorée et le programme continue (ou s'arrête silencieusement).

**Correction** :
```python
except (ValueError, IndexError) as e:
    print(f"**ERROR while reading {fname} at line: {line!r}: {e}")
    break
```

---

## PY-06 🟠 `res2vtp.py` : le fichier n'est pas fermé en cas d'exception

**Fichier** : [sph/res2vtp.py](../sph/res2vtp.py)

```python
file = open(fname)
# ...
for line in file:
    try:
        ...
    except:
        ...
        break
file.close()
```

Si une exception non catchée survient pendant l'itération, `file.close()` n'est jamais appelé, laissant le handle de fichier ouvert.

**Correction** : Utiliser `with` :
```python
with open(fname) as file:
    for line in file:
        ...
```

---

## PY-07 🟠 `helpers.py` : `Box.generate()` utilise `round()` pour les comptages

**Fichier** : [sph/helpers.py](../sph/helpers.py)

```cpp
ni = round(Lx / self.s)
```

`round()` en Python utilise le "banker's rounding" (arrondi à l'entier pair le plus proche pour les demi-entiers). Par exemple `round(0.5) == 0` en Python 3. Pour des divisions de boîtes, il serait plus intuitif et prévisible d'utiliser `max(1, int(Lx / self.s + 0.5))` ou `math.ceil`.

De plus, si `Lx` et `self.s` sont proches (e.g. `Lx = 0.049999...` et `s = 0.05`), `round()` donne `ni = 1` alors qu'on attendrait `ni = 1` aussi, mais des erreurs d'arrondi peuvent donner `0`, produisant une division par zéro dans `dx = Lx / ni`.

---

## PY-08 🟡 `run.py` : le fichier ouvert dans `run_simulation()` n'est pas fermé

**Fichier** : [run.py](../run.py)

```python
script = open(testname, encoding='utf-8').read()
```

Le handle de fichier est créé mais jamais explicitement fermé. Bien que le GC le fermera éventuellement, ce n'est pas une bonne pratique.

**Correction** :
```python
with open(testname, encoding='utf-8') as f:
    script = f.read()
```

---

## PY-09 🟡 `sph/helpers.py` : classe `Runner` appelle `clean()` et détruit les résultats sans confirmation

**Fichier** : [sph/helpers.py](../sph/helpers.py)

```python
def run(self):
    self.clean()  # supprime TOUS les fichiers res*.* du répertoire courant
    # ...
```

`clean()` supprime tous les fichiers `res*.*`, `input.*`, `grid.*` du répertoire de travail courant, sans confirmation. Si l'utilisateur lance un test par erreur dans un mauvais répertoire, des résultats pourraient être effacés silencieusement.

---

## PY-10 🟡 `sph/utils.py` : `setupwdir` change le répertoire courant globalement

**Fichier** : [sph/utils.py](../sph/utils.py)

```python
def setupwdir(testname):
    # ...
    os.chdir(wdir)
```

`os.chdir()` modifie le répertoire courant du **processus entier**. Tout le code qui suit utilise des chemins relatifs, ce qui est fragile si `setupwdir` n'est pas appelé correctement, ou si plusieurs tests sont lancés en parallèle dans le même processus.

C'est la raison pour laquelle `save_particles()` en C++ écrit dans le répertoire courant sans chemin absolu — c'est un couplage implicite fragile entre le code Python (qui change le répertoire) et le code C++ (qui en dépend).

---

## PY-11 🟡 Tests : pas de framework de test, pas de CI

Les "tests" dans `tests/*.py` sont des simulations complètes qui doivent être exécutées manuellement et évaluées visuellement dans Paraview. Il n'existe pas de :
- Tests unitaires (vérifier les kernels, l'équation d'état, le sorter...)
- Tests de régression avec valeurs de référence
- CI/CD pour valider les commits

```
run_tests.sh  ← lance 4 simulations complètes, sans assertion
```

---

## PY-12 🟡 `res2vtp.py` : conversion `vtkFloatArray` au lieu de `vtkDoubleArray`

**Fichier** : [sph/res2vtp.py](../sph/res2vtp.py)

```python
scalars = vtk.vtkFloatArray()
```

La simulation travaille en **double précision** (`double` en C++, `real*8` en Fortran). La conversion vers `float` (simple précision) lors de l'export VTK introduit une perte de précision qui peut être problématique pour des analyses quantitatives dans Paraview.
