# AI Context — Projet barres (dcm1)

Fichier de contexte à transmettre à une session IA pour reprendre le travail sur ce projet.

---

## Présentation

Application Qt Widgets C++ (Qt 5/6, Windows, MSVC) visualisant un **mécanisme à barres** en 2D.
Workspace : `d:\dev\progs\student\dcm1`

## Build

```powershell
cmake --build build --config Release
# produit : build/Release/barres.exe
```

---

## Structure des fichiers

| Fichier | Rôle |
|---|---|
| `Barres.h` / `Barres.cpp` | Widget Qt principal — animation, pan/zoom, API paramètres |
| `Window.h` / `Window.cpp` | QMainWindow — menus, sliders, raccourcis, dialogs |
| `MechanismRenderer.h` / `MechanismRenderer.cpp` | Rendu QPainter ; struct `RenderStyleSettings` |
| `MechanismKinematicsSolver.h` / `MechanismKinematicsSolver.cpp` | Calcul cinématique ; struct `MechanismParameters`, `TrajectoryGeometry` |
| `main.cpp` | Point d'entrée ; définit org/app name pour QSettings |

---

## Structures de données clés

### `MechanismParameters` (MechanismKinematicsSolver.h)
```cpp
struct MechanismParameters { double a1, a2, a3, xb, ya, L, e, dp; };
enum PointIndex { A=0, D=1, C=2, B=3, Pprime=4, P=5 };
```

### `RenderStyleSettings` (MechanismRenderer.h)
```cpp
struct RenderStyleSettings {
    double linkPenWidth = 2.0;
    double trajectoryPWidth = 2.0;
    double trajectoryDWidth = 0.5;
    double groundHalfLen = 0.5;   // unités monde
    double filmExtension = 10.0;  // unités monde
    int labelFontSize = 10;       // pt
    int labelOffsetX = 3;         // px
    int labelOffsetY = 3;         // px
    QColor mainColor = Qt::black;
    QColor trajectoryPColor = Qt::red;
    QColor trajectoryDColor = Qt::darkBlue;
};
```

### `MechanismRenderer::draw()`
```cpp
static void draw(QPainter &painter, const TrajectoryGeometry &geometry,
                 const MechanismParameters &params, int frame, int nframes,
                 int ox, int oy, double zoom,
                 const RenderStyleSettings &style, const QRect &widgetRect);
// sx(wx) = ox + wx*zoom    sy(wy) = oy - wy*zoom
```

---

## Classe `Barres` (widget de rendu)

### Membres privés importants
```cpp
static constexpr int nframes = 50;
double zoom;          // initial = 60.0
QPoint panOffset;     // initial = QPoint(150, 300)
QPoint lastMousePos;
bool isPanning;
MechanismParameters params;
RenderStyleSettings renderStyle;
TrajectoryGeometry geometryCache;
bool geometryDirty;
int myTimerId;
int frame;
```

### API publique
```cpp
MechanismParameters currentParameters() const;
void applyParameters(const MechanismParameters &newParams);
RenderStyleSettings currentRenderStyle() const;
void applyRenderStyle(const RenderStyleSettings &newStyle);
```

### Slots publics
```cpp
void set_a1_slot(int i);  // ...idem a2, a3, xb, ya, L, e, dp
void startAnimation();
void stopAnimation();
void toggleAnimation();
void resetAnimation();   // frame = 0
void resetView();        // zoom=60, panOffset=(150,300)
void stepForward();
void stepBackward();
```

### Signaux
```cpp
void animationStarted();
void animationStopped();
```

### Constantes de zoom (namespace anonyme dans Barres.cpp)
```cpp
constexpr double kMinZoom  = 10.0;
constexpr double kMaxZoom  = 400.0;
constexpr double kZoomStep = 1.1;
```

### Zoom centré sous la souris (wheelEvent)
```cpp
// Formule appliquée après clamp du zoom :
double r = newZoom / oldZoom;
panOffset.setX(qRound(mousePos.x() - (mousePos.x() - panOffset.x()) * r));
panOffset.setY(qRound(mousePos.y() - (mousePos.y() - panOffset.y()) * r));
```

---

## Classe `Window` (fenêtre principale)

### Sliders — factory lambda
```cpp
// Retourne QSlider* pour pouvoir synchroniser depuis l'import JSON
auto addSlider = [&](const QString &label, const QString &tooltip,
    double minValue, double maxValue,
    void (Barres::*slot)(int)) -> QSlider* { ... };

// Inverse (pour import JSON → slider)
auto toSliderValue = [](double value, double minValue, double maxValue) -> int
    { return qRound((value - minValue) / (maxValue - minValue) * 100); };
```

### 8 pointeurs de sliders nommés
```
sliderA1, sliderA2, sliderA3, sliderXb, sliderYa, sliderL, sliderE, sliderDp
```

### Menus
| Menu | Actions |
|---|---|
| `&Fichier` | Importer (Ctrl+O), Exporter (Ctrl+S), —, Quitter |
| `&Affichage` | Réinitialiser la vue (H), —, Paramètres de dessin... |
| `&Animation` | Démarrer, Arrêter |
| `&Aide` | A propos |

### Raccourcis clavier
| Touche | Action |
|---|---|
| `Space` | start/stop animation |
| `R` | reset frame (frame=0) |
| `←` / `→` | ±1 frame |
| `H` | reset vue (zoom+pan) |
| `Ctrl+O` | importer JSON |
| `Ctrl+S` | exporter JSON |

---

## QSettings

Identité (définie dans `main.cpp`) :
```cpp
QCoreApplication::setOrganizationName("rboman");
QCoreApplication::setApplicationName("barres");
```

Clés utilisées :
| Clé | Type | Usage |
|---|---|---|
| `io/lastParamsDir` | QString | Dernier répertoire import/export JSON |
| `view/zoom` | double | Niveau de zoom sauvé |
| `view/panOffsetX` | int | Décalage horizontal de la vue |
| `view/panOffsetY` | int | Décalage vertical de la vue |
| `render/linkPenWidth` | double | Style de dessin |
| `render/trajectoryPWidth` | double | |
| `render/trajectoryDWidth` | double | |
| `render/groundHalfLen` | double | |
| `render/filmExtension` | double | |
| `render/labelFontSize` | int | |
| `render/labelOffsetX` | int | |
| `render/labelOffsetY` | int | |
| `render/mainColor` | QString (HexArgb) | |
| `render/trajectoryPColor` | QString (HexArgb) | |
| `render/trajectoryDColor` | QString (HexArgb) | |

---

## Format JSON (import/export paramètres)

```json
{
  "format": "barres-params",
  "version": 1,
  "savedAt": "2026-03-13T10:00:00Z",
  "params": {
    "a1": 1.0, "a2": 1.0, "a3": 1.0,
    "xb": 1.0, "ya": 1.0, "L": 1.0,
    "e": 0.0,  "dp": 0.0
  }
}
```
L'import effectue une validation en 7 niveaux (format, version, type de chaque champ, bornes).

---

## Dialog "Paramètres de dessin"

Ouvert via menu `Affichage → Paramètres de dessin...`
4 onglets : **Traits** | **Labels** | **Couleurs** | *(potentiellement d'autres à ajouter)*
Boutons : OK / Annuler / Appliquer / Réinitialiser

---

## Fonctionnalités déjà implémentées (à ne pas refaire)

- [x] Raccourcis clavier (Space, R, ←, →, H)
- [x] Tooltips physiques sur les 8 sliders
- [x] Taille minimale fenêtre (900×580), layout stretch 4:1
- [x] Export/import JSON avec validation + sync sliders
- [x] Persistence dernier répertoire (`io/lastParamsDir`)
- [x] Dialog "Paramètres de dessin" avec QSettings persistence
- [x] 3 couleurs configurables (mainColor, trajectoryPColor, trajectoryDColor)
- [x] Pan souris gauche (curseur feedback)
- [x] Zoom molette centré sous le curseur (bornes 10–400, step 1.1)
- [x] Reset vue (`H` / menu)
- [x] Persistence pan/zoom (`view/zoom`, `view/panOffsetX`, `view/panOffsetY`)
