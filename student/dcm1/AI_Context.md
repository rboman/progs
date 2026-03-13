# AI Context — Projet barres (dcm1)

Contexte synthétique pour reprendre rapidement le projet dans une nouvelle session.

---

## Présentation

Application Qt Widgets C++ (Qt5, Windows/MSVC) visualisant un mécanisme à barres 2D.
Workspace: `d:\dev\progs\student\dcm1`

---

## Build / Run

```powershell
cmake -S . -B build
cmake --build build --config Release
# exe: build/Release/barres.exe
```

Tests:

```powershell
cmake --build build --config Release --target mechanism_kinematics_solver_test
build/Release/mechanism_kinematics_solver_test.exe
```

---

## Fichiers clés

| Fichier | Rôle |
|---|---|
| `main.cpp` | Init app + chargement traduction EN selon `ui/language` |
| `Window.h/.cpp` | Fenêtre principale, menus, sliders, import/export JSON, drag&drop |
| `Barres.h/.cpp` | Widget rendu + animation + pan/zoom + persistance vue/vitesse |
| `MechanismRenderer.h/.cpp` | Rendu QPainter, grille monde, overlay frame/angles |
| `MechanismKinematicsSolver.h/.cpp` | Calcul cinématique, trajectoires |
| `CMakeLists.txt` | Build app/tests + pipeline Qt Linguist (`.ts/.qm`) |
| `i18n/barres_en.ts` | Traductions EN |

---

## Fonctionnalités en place

- Animation avec start/stop/toggle, step +/-1 frame, reset frame.
- Contrôle de vitesse animation (`5..200 ms/frame`), persistant.
- Pan souris gauche + zoom molette centré curseur (bornes `10..400`).
- Reset vue (`H` ou menu), vue persistée (`zoom` + `panOffset`).
- Grille de fond adaptative au zoom + overlays (`frame`, angles AD/DC/BC).
- Import/export JSON des paramètres (validation stricte + synchro sliders).
- Drag&drop de fichiers `.json` pour import.
- Dialog "Paramètres de dessin" (traits/labels/couleurs) avec persistance.
- Taille fenêtre persistée.

---

## i18n (état actuel)

- UI passée en `tr(...)` (classes QObject) et `QCoreApplication::translate(...)` pour `MechanismRenderer`.
- Langues supportées: `fr` (source par défaut), `en` (via `barres_en.qm`).
- Sélecteur menu: `Affichage -> Langue -> Francais / Anglais`.
- Changement de langue appliqué au prochain démarrage (pas de hot-reload).

Pipeline traduction (CMake):

- `find_package(Qt5 COMPONENTS Widgets LinguistTools REQUIRED)`
- génération `.qm` depuis `i18n/barres_en.ts`
- copie post-build vers `build/Release/i18n/barres_en.qm`

Note: le build peut mettre à jour automatiquement `i18n/barres_en.ts`.

---

## Structures / API utiles

```cpp
struct MechanismParameters { double a1, a2, a3, xb, ya, L, e, dp; };
enum PointIndex { A=0, D=1, C=2, B=3, Pprime=4, P=5 };
```

`Barres` (API la plus utile):

```cpp
MechanismParameters currentParameters() const;
void applyParameters(const MechanismParameters &newParams);
RenderStyleSettings currentRenderStyle() const;
void applyRenderStyle(const RenderStyleSettings &newStyle);
int currentAnimationIntervalMs() const;
void setAnimationIntervalMs(int intervalMs);
```

---

## QSettings utilisés

Identité app:

```cpp
QCoreApplication::setOrganizationName("rboman");
QCoreApplication::setApplicationName("barres");
```

Clés:

- `io/lastParamsDir` (QString)
- `window/size` (QSize)
- `ui/language` (`fr`/`en`)
- `view/zoom` (double)
- `view/panOffsetX`, `view/panOffsetY` (int)
- `animation/intervalMs` (int)
- `render/*` (`linkPenWidth`, `trajectoryPWidth`, `trajectoryDWidth`, `groundHalfLen`, `filmExtension`, `labelFontSize`, `labelOffsetX`, `labelOffsetY`, `mainColor`, `trajectoryPColor`, `trajectoryDColor`)

---

## Format JSON import/export

```json
{
  "format": "barres-params",
  "version": 1,
  "savedAt": "ISO-8601",
  "params": {
    "a1": 0.0, "a2": 0.0, "a3": 0.0,
    "xb": 0.0, "ya": 0.0, "L": 0.0,
    "e": 0.0,  "dp": 0.0
  }
}
```

Validation import: format + version + présence/type + bornes de chaque paramètre.

Bornes UI/import:

- `a1` `[0.1, 2.0]`
- `a2` `[2.5, 4.5]`
- `a3` `[1.0, 3.0]`
- `xb` `[2.0, 4.0]`
- `ya` `[0.5, 2.5]`
- `L` `[4.0, 8.0]`
- `e` `[0.0, 3.0]`
- `dp` `[0.5, 1.5]`

---

## Raccourcis clavier

- `Space`: start/stop animation
- `R`: reset frame
- `Left` / `Right`: frame -/+ 1
- `H`: reset vue
- `Ctrl+O`: importer JSON
- `Ctrl+S`: exporter JSON
