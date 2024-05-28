# Notes SPH

## TFE Louis

### Introduction

SPH: smoothed particle hydrodynamics: meshfree - Lagrangian - particle method

### Représentation intégrale de $f$ et de sa dérivée

Représentation exacte de $f$ définie sur $\Omega$:
$$
f(\boldsymbol{x}) = \int_{\Omega}f(\boldsymbol{x'})
\,\delta(\boldsymbol{x}-\boldsymbol{x'})\,dV_{\boldsymbol{x'}}
$$
Approximation: on remplace le Dirac $\delta$ par une fonction de lissage $W$ (anglais: *smoothing function*, *smoothing kernel function*, *kernel function*, *kernel*):
$$
\langle f(\boldsymbol{x})\rangle = \int_{\Omega}f(\boldsymbol{x'})
\,W(\boldsymbol{x}-\boldsymbol{x'}, h)\,dV_{\boldsymbol{x'}}
$$
où $h$ est un paramètre nommé "longueur de lissage" (*smoothing length*). $h_0$ est choisie comme un multiple de la distance entre particules noté $s_0$ ($h_0\in[1s_0, 2 s_0]$).

Le support de $W$ est de diamètre $2\kappa h$.

Approximation de la dérivée d'une fonction vectorielle $\boldsymbol{f}$:
$$
\begin{aligned}
&&\langle \nabla \cdot \boldsymbol{f}(\boldsymbol{x})\rangle 
   &= \int_{\Omega}
   \nabla_{\boldsymbol{x'}}\cdot \boldsymbol{f}(\boldsymbol{x'})
   \,W(\boldsymbol{x}-\boldsymbol{x'}, h)\,dV_{\boldsymbol{x'}} \\
&& &= \int_{\partial\Omega}
   \left[ \boldsymbol{f}(\boldsymbol{x'})\, W(\boldsymbol{x}-\boldsymbol{x'}, h)\right]
   \cdot \boldsymbol{n} \, dS_{\boldsymbol{x'}}
   - \int_{\Omega}
    \boldsymbol{f}(\boldsymbol{x'})
   \,\nabla_{\boldsymbol{x'}} \cdot W(\boldsymbol{x}-\boldsymbol{x'}, h)\,dV_{\boldsymbol{x'}} \\
 && &= 
   - \int_{\Omega}
    \boldsymbol{f}(\boldsymbol{x'})
   \,\nabla_{\boldsymbol{x'}} \cdot W(\boldsymbol{x}-\boldsymbol{x'}, h)\,dV_{\boldsymbol{x'}} \qquad\text{loin de la surface.} 
\end{aligned}
$$

Si $f$ est une fonction scalaire, on utilise le [théorème du gradient](https://fr.wikipedia.org/wiki/Th%C3%A9or%C3%A8me_du_gradient) ($\int_V \nabla p\,dV = \oint_S p\, \boldsymbol{n}\,dS$) au lieu de la divergence, pour obtenir une expression similaire:
$$
\langle \nabla f(\boldsymbol{x})\rangle
=
- \int_{\Omega}
    f(\boldsymbol{x'})
   \,\nabla_{\boldsymbol{x'}} W(\boldsymbol{x}-\boldsymbol{x'}, h)\,dV_{\boldsymbol{x'}} \qquad\text{loin de la surface.}
$$

### Approximation particulaire

Hypothèse: chaque particule a une masse constante $m_b$ au cours de la simulation (où $b$ désigne une particule). 

Sa densité $\rho_b$ (et donc son "volume" $\Delta V_b$ ) peut varier.
$$
m_b = \rho_b \,\Delta V_b \qquad \Rightarrow\quad \Delta V_b = \frac{m_b}{\rho_b}
$$
Dans la suite, on considère la particule $a$ et ses voisins indexés par $b$ (les intégrales deviendront des sommes sur $b$).
$$
\begin{aligned}
&& \langle f(\boldsymbol{x}_a)\rangle &= \int_{\Omega}f(\boldsymbol{x'})
\,W(\boldsymbol{x_a}-\boldsymbol{x'}, h)\,d\boldsymbol{x'} 
 \\
&& &\approx \sum_{b=1}^N f(\boldsymbol{x}_b) \, W(\boldsymbol{x_a}-\boldsymbol{x_b}, h)\,\Delta V_b
 \\
&& &= \sum_{b=1}^N f(\boldsymbol{x}_b) \, W(\boldsymbol{x_a}-\boldsymbol{x_b}, h)\,\frac{m_b}{\rho_b}
\\
&& &= \sum_{b=1}^N \frac{m_b}{\rho_b} \, f(\boldsymbol{x}_b) \, W_{ab}
\end{aligned}
$$
avec $W_{ab} = W(\boldsymbol{x_a}-\boldsymbol{x_b}, h)$. On note aussi $r_{ab}=|\boldsymbol{x_a}-\boldsymbol{x_b}|$.

$N$ est le nombre de particules du problème. En pratique, on se limitera à la particule elle-même (car $W(0,h)\neq 0$) et aux voisins proches pour lesquels $W\neq 0$.

De même, pour la divergence d'une fonction vectorielle:
$$
\begin{aligned}
&&\langle \nabla \cdot \boldsymbol{f}(\boldsymbol{x_a})\rangle 
   & = - \sum_{b=1}^N \frac{m_b}{\rho_b} \, \boldsymbol{f}(\boldsymbol{x}_b) \, \nabla_b \cdot W_{ab} 
\end{aligned}
$$
Note: $\nabla_b \cdot W_{ab} = - \nabla_a \cdot W_{ab}$

Gradient d'une fonction scalaire:
$$
\langle \nabla f(\boldsymbol{x_a})\rangle = \sum_{b=1}^N \frac{m_b}{\rho_b} \, f(\boldsymbol{x}_b) \, \frac{\boldsymbol{x_a}-\boldsymbol{x_b}}{|\boldsymbol{x_a}-\boldsymbol{x_b}|} \left.\frac{dW}{dr}\right|_{r=r_{ab}}
$$


On utilise une valeur de $h$ identique pour toutes les particules pour vérifier automatiquement le principe d'action/réaction.

### Fonctions de lissage

La fonction $W(r,h)$ de lissage ou kernel doit:

* être normalisée (son intégrale spatiale vaut 1),
* idéalement être définie sur un compact,
* être positive sur son domaine de support,
* décroitre avec $r$,
* tendre vers un Dirac si $h$ tend vers 0,
* être symétrique,
* être lisse.

Kernels courants: Gaussian, Bell-shaped, Cubic-spline, Quadratic, Quintic, Quintic-spline.

Attention: il y a des erreurs dans les expressions de Louis (cfr. coefficients de $W$ pour cubic-spline).

### Navier-Stokes

Calcul de la densité (via approx particulaire de $\rho(\boldsymbol{x})$ - *summation density*):
$$
\rho_a = \sum_{b=1}^N m_b \, W_{ab}
$$
* Continuité / conservation de la masse:

$$
\frac{D \rho}{D t} = -\rho \, \nabla\cdot\boldsymbol{u} = \boldsymbol{u}\cdot\nabla \rho -\nabla\cdot (\rho\,\boldsymbol{u})
$$

$$
\underbrace{\frac{D \rho}{D t}}_{a} = \underbrace{\boldsymbol{u}}_{a}\cdot\underbrace{\nabla \rho}_{\sum_b} -\underbrace{\nabla\cdot (\rho\,\boldsymbol{u})}_{\sum_b}
$$

$$
\boxed{ \frac{D \rho_a}{D t} = \sum_{b=1}^N m_b \, \boldsymbol{u}_{ab}\,\nabla_aW_{ab} }
$$

avec $\boldsymbol{u}_{ab}=\boldsymbol{u}_a-\boldsymbol{u}_b$. Cette équation traduit le fait que 2 particules qui se rapprochent voient leur densité augmenter (qui produira une augmentation de pression).

* Conservation de la quantité de mouvement

$$
\rho\frac{D\boldsymbol{u}}{D t} 
= -\nabla p + \nabla\cdot\boldsymbol{T} + \rho \boldsymbol{F}
$$

$$
\Rightarrow \frac{D\boldsymbol{u}}{D t} 
= - \left( \nabla\frac{p}{\rho}+\frac{p}{\rho^2}\nabla\rho\right) + \frac{1}{\rho}\nabla\cdot\boldsymbol{T} + \boldsymbol{F}
$$

! Attention $\boldsymbol{F}$ n'est pas une force mais une accélération !

$$
\underbrace{ \frac{D\boldsymbol{u}}{D t} }_{a}
= - \left( \underbrace{ \nabla\frac{p}{\rho} }_{\sum_b}
  + \underbrace{\frac{p}{\rho^2}}_{a} \underbrace{  \nabla\rho }_{\sum_b}\right) + \frac{1}{\rho}\nabla\cdot\boldsymbol{T} + \boldsymbol{F}
$$

$$
\boxed{ \frac{D\boldsymbol{u_a}}{D t} = - \sum_{b=1}^N m_b 
\left( \frac{p_b}{\rho_b^2}+ \frac{p_a}{\rho_a^2} + \Pi_{ab} \right) \nabla_a W_{ab} + \boldsymbol{F} }
$$

### Equation d'état

La pression n'est pas une variable particulaire. Elle se déduit de la densité courante en évaluant une équation d'état.

* Gaz parfait:

$$
p = \frac{R\, T}{M}\left( \frac{\rho}{\rho_0}-1\right)
$$

avec $\rho = \rho_0$ implique $p=0$

* Fluide quasi-incompressible:

$$
p = B\left( \left(\frac{\rho}{\rho_0}\right)^{\gamma} -1 \right)
$$

avec $\gamma\approx 7$ (eau de mer) et $B=\frac{c_0^2\rho_0}{\gamma}$ avec $c_0=1480 \, m/s$ pour l'eau.

Approximation linéaire (ne pas utiliser pour $c_0$ faible):
$$
p = c_0^2\,\rho_0 \left( \frac{\rho}{\rho_0} -1 \right)
$$
La *vitesse du son* $c$ se calcule en dérivant l'expression choisie à entropie constante:
$$
c^2 = \left.\frac{\partial p}{\partial \rho}\right|_{s}
$$
On obtient pour le fluide quasi-incompressible:
$$
c=c_0\sqrt{\left( \frac{\rho}{\rho_0}\right)^{\gamma-1}}
$$


Notes: la pression n'étant pas une variable de la particule, la pression initiale peut être fixée en inversant l'équation d'état et en calculant une densité initiale $\rho(t=0)$

Diminuer la vitesse du son (pour augmenter le pas de temps - voir plus loin) provoque une augmentation de la compressibilité et donc une erreur sur le volume du fluide soumis à la gravité.

Une règle de bonne pratique est d'utiliser $c_0 = 10\sqrt{gH}$ pour les problèmes "shallow water waves".

### Viscosité

Il s'agit d'une viscosité numérique pour stabiliser le système, n'ayant aucune réalité physique. 
$$
\Pi_{ab} = \left\{ 
\begin{aligned}
&\frac{-\alpha\,\bar{c}_{ab}\,\mu_{ab}+\beta \,\mu_{ab}^2}{\bar{\rho}_{ab}} & \text{ for } & \boldsymbol{u}_{ab}\cdot(\boldsymbol{x}_a-\boldsymbol{x}_b)<0 
\quad\text{($a$ et $b$ se rapprochent)}\\
& 0 & \text{ for } & \boldsymbol{u}_{ab}\cdot(\boldsymbol{x}_a-\boldsymbol{x}_b)\ge 0 
\quad\text{($a$ et $b$ s'éloignent)}
\end{aligned}
\right.
$$
avec $\bar{c}_{ab}$ la vitesse du son moyenne des 2 particules; $\bar{\rho}_{ab}$, la densité moyenne et
$$
\mu_{ab} = \frac{h \,\boldsymbol{u}_{ab}\cdot (\boldsymbol{x}_a-\boldsymbol{x}_b)}{(\boldsymbol{x}_a-\boldsymbol{x}_b)^2+\eta^2}
$$
avec $\eta = 0.01\, h^2$

Valeurs courantes: $\alpha=1$ et $\beta=2$ (Monaghan) ou $\alpha\in[0.01,0.5]$ et $\beta=0$

"La force créée empêche l'éloignement des particules" dit Louis plus tard (ça semble être l'inverse). D'après Louis, provoque l'apparition de particules "collées" aux parois du test.

! il y a un problème dans la formule de Louis: $\mu_{ab}$ est toujours négatif ! donc $\max_b \mu_{ab}$ sera nul dans le calcul du pas de temps plus tard (pas un souci majeur puisque ce terme est multiplié par un facteur $\beta$ qui est généralement nul).

Monagan 1989 définit $\mu_{ab}$ avec un signe "-" devant l'expression ci-dessus. Le "-" disparait alors dans l'équation de $\Pi_{ab}$



### Longueur de lissage

Généralement $s\in[1s, 2s]$. Choix de Louis = celui du Liu: $h = 1.2 s$ avec $s$ est l'espacement des particules (supposé constant). Ca conduit à 2 voisins de chaque côté à 1D (pour les kernels $\kappa=2$).

Mise à jour globale de $h$ à partir de la densité moyenne $\rho_{\text{moy}}$:
$$
h=h_0 \left( \frac{\rho_0}{\rho_{\text{moy}}} \right)^{\frac{1}{d}}
$$
où $d$ est la dimension.

### Recherche des voisins

Louis propose d'utiliser une grille de taille $\kappa\, h$ et de classer les particules dans cette grille. Les voisins sont ensuite recherchés uniquement dans la cellule et les 8 cellules adjacentes. C'est la méthode la plus rapide et la plus simple pour $h$ spatialement constant.

### Précision des calculs

Note: il est nécessaire d'utiliser des double précision si $c_0 = 1420 \,m/s$, sinon, 2 particules voisines peuvent avoir la même pression tronquée à 7 chiffres significatifs. 

J-F Remacle m'a dit qu'il est possible de réécrire les équations pour utiliser des nombres "simple précision".

### Normalisation du kernel

Idéalement on a 
$$
1 = \sum_{b=1}^N \frac{m_b}{\rho_b} W_{ab}
$$
Possibilité de corriger le kernel lui-même ou son gradient. Cela entraine la résolution d'un système 3x3. cela provoque également une perte de symétrie (et la violation du principe d'action/réaction) pour 2 particules proches de la surface. Cette technique est à appliquer uniquement pour l'équation de la quantité de mouvement. D'autres méthodes existent pour l'équation de continuité (pas implémentées par Louis).

### Conditions aux limites

Louis utilise des "*particules dynamiques*". Ce sont des particules pour lesquelles l'équation de quantité de mouvement n'est pas résolue. Elles ont une vitesse imposée (à 0 ou autre valeur).

Parfois 2 couches imbriquées peuvent être nécessaires (évite une approx. particulaire incomplète pour les particules proches de la frontière).

### Initialisation

* On utilise une grille cartésienne de pas $s$.
* La pression hydrostatique est calculée. 
* On en déduit la densité en inversant l'équation d'état.

$$
\rho(z)=\rho_0\left( 1+\frac{1}{B}\rho_0 g z\right)^{\frac{1}{\gamma}}
$$

* La masse est calculée en multipliant la densité par le volume $s^d$ (où $d$ est la dimension).

### Intégration temporelle

On calcule ces 2 pas de temps (voir livre Liu p 142)
$$
\Delta t_f = \min_a \sqrt{\frac{h_a}{|\boldsymbol{F}_a|}}
$$

$$
\Delta t_{cv} = \min_a \left( \frac{h_a}{c_a+0.6\,(\alpha \, c_a+\beta\,\max_b \mu_{ab})} \right)
$$

d'après [Theory - Time Step Size — DualSPHysics Forums](https://forums.dual.sphysics.org/discussion/1808/theory-time-step-size) il faut une valeur absolue autour de $\mu_{ab}$ (logique puisque $\mu_{ab}$​ de Louis est négatif):

Monagan-1989 / Liu-2003 utilisent 1.2 au lieu de 0.6. et multiplient le tout par un nombre de Courant de 0.3...

Et on les combine:
$$
\Delta t = \min \left( 0.25 \Delta t_f, 0.4 \Delta t_{cv}\right)
$$
Note: la formule est celle de Monaghan-1989, mais Louis la code en inversant les facteurs:
$$
\Delta t = \min \left( 0.4 \Delta t_f, 0.25 \Delta t_{cv}\right)
$$
ce qui provoque un plus petit pas de temps, car généralement le terme $cv$ est le plus petit des 2.

L'intégrateur temporel est RK22. Si $y' = f(t,y)$
$$
\left\{
\begin{aligned}
y_{i+1} &= y_i +  \frac{\Delta t}{2} (k_1+k_2) \\
k_1 &= f(t_i, y_i) \\
k_2 &= f(t_i+ \Delta t, y_i+ \Delta t\,k_1)
\end{aligned}
\right.
$$

Si on pose $\boldsymbol{V} = \left( \boldsymbol{u}\; \boldsymbol{x}\;\rho \right)$, le système s'écrit:
$$
\boldsymbol{V}=\boldsymbol{F}(\boldsymbol{V}(t))
$$


### Cas tests de vérification

* **Particles fall**: chute d'un cube contre un plancher. Permet de vérifier le mouvement uniformément accéléré, les symétries de la solution. 1-5s CPU pour ~1k particules et 1.5s simulées ($c_0=30\,m/s$).
* **Water in a still tank**: vérification de l'équilibre. On remarque des problèmes près des bords (pas de correction du kernel). 75min CPU pour ~40k particules et 5s simulées ($c_0=30\,m/s$).
* **Dam break on a dry bed**:  Le champ de pression est bruité. Mais le mouvement du fluide est globalement bien représenté. CPU: 2.9h pour 180k particules et 2s simulées ($c_0=35\,m/s$).
* **Dam break on a wet bed**: La vitesse de propagation du front est comparée à la solution analytique. CPU: 23.5min pour 50k particules et 1.5s simulées ($c_0=15\,m/s$).
* **Spinning tank**: On regarde la forme de la surface libre. CPU: 4.5h pour $c_0=50\,m/s$ et 109.8h pour $c_0=1480\,m/s$ - 7s simulées ~55k particules.

### Etude de paramètres

* **Kernels**: le kernel "quintic spline" produit des temps CPU 2.5x supérieurs aux autres (car plus de particules - $\kappa=3$). Louis recommande le kernel "cubic spline", plus stable selon ses tests.
* **Schémas d'intégration**: RK22 provoque une augmentation CPU de 25% (pas 100% car la recherche des voisins n'est faite qu'une fois pour les 2 pas). Euler donne des résultats un peu moins bons mais très proches de RK22 (dam break).
* **Condition aux limites de "symétrie"**: on gère le contact avec des particules de l'autre côté du mur. Les résultats sont meilleurs.
* **Correction du kernel**: Le temps CPU augmente de 40%. Résultats très légèrement meilleurs.

### Tests avancés

* **Dam break followed by a jump**: CPU: 4.1h pour 175k particules et 1.8s simulées ($c_0=70\,m/s$).
* **Dam break through a grid**: CPU: 1.3h pour 180k particules et 1s simulées ($c_0=35\,m/s$).
* **3-D dam break**: CPU: 1.8h pour 140k particules et 2s simulées ($c_0=35\,m/s$).
* **Structure impact**: dam break contre un mur partiellement ouvert. CPU: 2.7h pour 190k particules et 2s simulées ($c_0=35\,m/s$).



### Notes

* Le domaine de calcul de Louis est cubique et non parallélépipédique.
* Pas de correction de kernel dans l'équation de continuité. Louis suggère un "density filter".
* applications possibles:
  * impact de structures avec ouvertures (fenêtres)
  * impact de 2 jets
  * vagues sur structures offshores - tempêtes
* autres applis suggérées par les étudiants 2017:
  * tension de surface
  * fluides non-newtoniens
  * interactions fluide/structure

## A regarder

* [InteractiveComputerGraphics/CompactNSearch: ](https://github.com/InteractiveComputerGraphics/CompactNSearch) A C++ library to compute neighborhood information for point clouds within a fixed radius.