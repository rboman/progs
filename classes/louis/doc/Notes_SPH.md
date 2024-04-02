# Notes SPH

## TFE Louis (2013)

### Représentation intégrale d'une fonction $f$

$$
\boldsymbol{f}(\boldsymbol{x}) = \int_{V}\boldsymbol{f}(\boldsymbol{x}') \, \delta (\boldsymbol{x}-\boldsymbol{x}')\,d\boldsymbol{x}'
$$
on remplace le Dirac $\delta (\boldsymbol{x}-\boldsymbol{x}')$ par une fonction lisse $W(\boldsymbol{x}-\boldsymbol{x}')$


$$
\langle \boldsymbol{f}(\boldsymbol{x}) \rangle 
= \int_{V}^{}{\boldsymbol{f}( \boldsymbol{x}' ) 
\, W(\boldsymbol{x} - \boldsymbol{x}', h)
\, dV_{\boldsymbol{x}'}}
$$

où $h$ est la *longueur de lissage* (*smoothing length*). Valeur initiale courante: $s_{0} \leq h_{0} \leq 2s_{0}$ $(h_{0} = 1.2s_{0})$ si $s_{0}$ est l'espacement initial des particules.

On en déduit une expression de la divergence de $\boldsymbol{f}$ :

$$
\langle \boldsymbol{\nabla}_{\boldsymbol{x}}\cdot \boldsymbol{f}(\boldsymbol{x}) \rangle 
= 
\int_{S}^{}{
    [ \boldsymbol{f}( \boldsymbol{x}' )\, W( \boldsymbol{x} - \boldsymbol{x}',h ) ]
\cdot d\boldsymbol{S}_{\boldsymbol{x}'}} 
- 
\int_{V}^{}{\boldsymbol{f}( \boldsymbol{x}' )
\cdot 
\boldsymbol{\nabla}_{\boldsymbol{x}'} W( \boldsymbol{x} - \boldsymbol{x}',h)\, dV_{\boldsymbol{x}'}}
$$

Le terme surfacique est nul sauf près de la frontière. Autrement dit :
$$
\langle \boldsymbol{\nabla}_{\boldsymbol{x}}\cdot \boldsymbol{f}(\boldsymbol{x}) \rangle =  - \int_{V}^{}{\boldsymbol{f}( \boldsymbol{x}' )\cdot \boldsymbol{\nabla}_{\boldsymbol{x}'} W( \boldsymbol{x} - \boldsymbol{x}',h)\, dV_{\boldsymbol{x}'}}
$$
Remarque: le gradient de $W$ est relatif à $\boldsymbol{x}'$ et non $\boldsymbol{x}$ !

### Approximation particulaire

On considère la masse d'une partricule $b$ constante. Sa densité $\rho_b$, par contre varie avec le temps et l'espace occupé par la particule $\Delta V_b$ varie également.
$$
m_{b} = \rho_{b}\ {\mathrm{\Delta}V}_{b}\  \Rightarrow \ {\mathrm{\Delta}V}_{b} = \frac{m_{b}}{\rho_{b}}
$$

On approxime alors l'intégrale de la représentation lissée de $\boldsymbol{f}$ 

$$
\left\langle \boldsymbol{f}(\boldsymbol{x}) \right\rangle = \int_{V}^{}{\boldsymbol{f}\left( \boldsymbol{x}\boldsymbol{'} \right)\ W\left( \boldsymbol{x} - \boldsymbol{x}',h \right)\ dV_{\boldsymbol{x}\boldsymbol{'}}}
$$
par
$$
\left\langle \boldsymbol{f}(\boldsymbol{x}) \right\rangle 
\approx 
\sum_{b} \underbrace{ \frac{m_{b}}{\rho_{b}}
\, W( \boldsymbol{x} - \boldsymbol{x}_{\boldsymbol{b}},h )}_{\text{poids}}
\  \boldsymbol{f}( \boldsymbol{x}_{b})
$$

Autrement dit: la valeur d'une fonction est approximée par une somme pondérée des valeurs des particules voisines du point où on veut évaluer la fonction. Le poids de chaque particule fait intervenir sa distance au point considéré (via la fonction de lissage $W$ qui décroit avec la distance) et son volume (masse constante sur densité variable).

$$
r_{ab} = \left\| \boldsymbol{x}_{\boldsymbol{a}} - \boldsymbol{x}_{\boldsymbol{b}} \right\|
$$

$$
W_{ab} = W(\boldsymbol{x}_{\boldsymbol{a}} - \boldsymbol{x}_{\boldsymbol{b}},h)
$$

$$
W\left( \boldsymbol{x},\boldsymbol{x}^{'},h \right) = \ W\left( \boldsymbol{x} - \boldsymbol{x}^{'},h \right)
$$

$$
\boldsymbol{\nabla}_{\boldsymbol{x}'}W\left( \boldsymbol{x}_{\boldsymbol{a}},\boldsymbol{x}_{\boldsymbol{b}},h \right) = \ {\boldsymbol{\nabla}_{b}W}_{ab}\boldsymbol{= -}{\boldsymbol{\nabla}_{a}W}_{ab}
$$

$$
{\boldsymbol{\nabla}_{a}W}_{ab}\boldsymbol{=}\frac{\boldsymbol{x}_{\boldsymbol{a}} - \boldsymbol{x}_{\boldsymbol{b}}}{r_{ab}}\ \left. \ \frac{dW}{dr} \right|_{r = r_{ab}}
$$

Equations

$$
\frac{D\rho}{Dt} = - \rho\ \boldsymbol{\nabla}.\boldsymbol{u}\boldsymbol{\ \ } \Rightarrow \ \ \frac{D\rho_{a}}{Dt} = \sum_{b}^{}{m_{b}\ \boldsymbol{u}_{ab}\boldsymbol{\ }{\boldsymbol{\nabla}_{a}W}_{ab}}
$$

$$
\rho\frac{D\boldsymbol{u}}{Dt} = - \boldsymbol{\nabla}p + \boldsymbol{\nabla}.\boldsymbol{T +}\rho\boldsymbol{F}\boldsymbol{\ } \Rightarrow \ \ \frac{D\boldsymbol{u}_{a}}{Dt} = - \sum_{b}^{}{m_{b}\left( \frac{p_{b}}{\rho_{b}^{2}} + \frac{p_{a}}{\rho_{a}^{2}} + \Pi_{ab} \right){\boldsymbol{\nabla}_{a}W}_{ab}} + \boldsymbol{F}
$$

Eq of state (ideal gas)

$$
p(\rho) = \frac{RT}{M}\left( \frac{\rho}{\rho_{0}} - 1 \right)
$$

$$c = c_{0}$$

Eq of state (quasi incompr fluid)

$$
p(\rho) = \frac{c_{0}^{2}\rho_{0}}{\gamma}\left( \left( \frac{\rho}{\rho_{0}} \right)^{\gamma} - 1 \right)
$$

$$
c = c_{0}\sqrt{\left( \frac{\rho}{\rho_{0}} \right)^{\gamma - 1}}
$$

density at t=0

$$
\rho(h) = \rho_{0}\left( 1 + \frac{M}{RT}\rho_{0}gh \right)
$$

$$
\rho(h) = \rho_{0}\left( 1 + \frac{\gamma}{c_{0}^{2}\rho_{0}}\rho_{0}gh \right)^{\frac{1}{\gamma}}
$$

Artificial viscosity

$\Pi_{ab} = \frac{- \alpha\ {\overline{c}}_{ab}\mu_{ab} + \beta\mu_{ab}^{2}}{{\overline{\rho}}_{ab}}$
for
$\boldsymbol{u}_{ab}.\ \left( \boldsymbol{x}_{a} - \boldsymbol{x}_{b} \right) < 0$
(particles move away from each other)

And 0 otherwise

$$
{\overline{c}}_{ab} = \frac{1}{2}\left( c_{a} + c_{b} \right)
$$

$$
{\overline{\rho}}_{ab} = \frac{1}{2}\left( \rho_{a} + \rho_{b} \right)
$$

$$
\mu_{ab} = \frac{h\ \boldsymbol{u}_{ab}.\ \left( \boldsymbol{x}_{a} - \boldsymbol{x}_{b} \right)\ }{\left( \boldsymbol{x}_{a} - \boldsymbol{x}_{b} \right)^{2} + \eta^{2}}
$$

$\eta = 0.01{\ h}^{2}$

Update of $h$

$$
h = h_{0}\left( \frac{\rho}{\rho_{0}} \right)^{1/d}
$$

d=2 en 2D; d=3 en 3D







## A regarder

* [InteractiveComputerGraphics/CompactNSearch: ](https://github.com/InteractiveComputerGraphics/CompactNSearch) A C++ library to compute neighborhood information for point clouds within a fixed radius.
