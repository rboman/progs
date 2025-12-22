# Mémo Julia

Pas de julia dans les packages Ubuntu 18.04 => télécharger les binaires linux.

Gros changements entre version 0.x et 1.x (aout 2018!)

Très lent au démarrage! https://www.zverovich.net/2016/05/13/giving-up-on-julia.html

- l'utilisation de julia est idéale dans des notebooks jupyter ou autres ou le temps des "`using XXX`" n'est compté qu'une seule fois
- au final, tracer un simple graphe avec Plots prends plus de 25s (pour 1-2s avec python/matplotlib)
- inutilisable sous forme de scripts "`julia plot.jl`"

Similarités avec MATLAB et FORTRAN

## Liens

* https://techytok.com/from-zero-to-julia/ (<= très bien)
* https://syl1.gitbook.io/julia-language-a-concise-tutorial/
* https://benlauwens.github.io/ThinkJulia.jl/latest/book.html


## Mémo

S'installe dans `C:\Users\r_bom\AppData\Local\Programs\Julia 1.5.3`

Les packages s'installent dans `C:\Users\r_bom\.julia`

quitter: `exit()` ou CTRL-D
`]`      : entre dans le package manager (CTRL-C pour sortir)

beaucoup de choses **comme MATLAB**:
- ";" en interactif supprime l'output
- les index démarrent à 1!
- slices: `a[2:3]`

similarité avec le **FORTRAN**
- les tableaux sont ordonnés colonne par colonne (=> boucles `for j`, `for i`)
- les "modules" font penser au fortran. Les méthodes liées aux objets (types) sont définies en dehors des objets. 

par contre:
- les variables sont des pointeurs comme en python (`a=b`, `a[2]=...` modifie `b`) 
- pas besoin de coder avec des opérations matricielles => utiliser des boucles "`for j,i`" est OK!

```julia
import Pkg; Pkg.add("Plots")  # telecharge une chiée de packages dont "Plots"
using Plots                   # précompile Plots la 1ere fois (long!)
import Pkg; Pkg.add("PyPlot") 
pyplot()                      # => installe matplotlib, qt, 
                              # pyqt et passe en backend pyplot pour Plots
plot(rand(4,4))
```

`?`   => passe en mode "help"

```julia
pwd()       # print working directory
cd("dir")   # change dir
```


### types

```
a = 2.0
typeof(a)    => Float64
convert(Int64, a)    # => 2
```

### functions

```julia
"""
documentation de f
"""
function f(x)
    return 2*x*x
end

f(x) = 2*x*x   # version courte
f(x) = 2x^2    # version très courte!
```

lorsqu'une fct ne retourne rien (pas de return), elle retourne la dernière expression calculée 

```julia
function f(x)
    2*x*x
end
```


### arrays

```
a=[1,2,3]  # démarre à 1 - ne contient qu'1 seul type!
append!(a,4)   # ajoute 4 (le "!" signifie que a sera changé)

mat = [1 2 3; 4 5 6]   # mat[1,2] accede à l'élément 1,2

[i for i in 1:20]    # list comprehension

a=b         # ! pointeurs comme en python
a=copy(b)   # solution: copie
length(a)   # taille
1:20        # = iterator
a = collect(1:20) # transforme l'iterateur en array
a = zeros(2,2)  # matrice 2x2 de zeros

d = reshape([1,2,3,4,5,6,7,8,9],3,3)
```


### tuples

similaires à python

```
a = 1,2
a = (1,2)        # idem
print("a=$a")    # impression
```

### dictionnaires

```
person1 = Dict("Name" => "Aurelio", "Phone" => 123456789, "Shoe-size" => 40)

### 
```

### control flow

très similaire à python mis à part la présence de "end" pour delimiter les blocs

```
for i in 1:5
   println(i)
end

if i>2 & i < 10
   ...
elseif
   ...
else
   ...
end
```


enumerate() # idem à python (retourne un tuple)

### arrays operations

```
a = [1,2,3]  # column vector
b = [4 5 6]  # row vector

a*b => 3×3 Array{Int64,2}
b*a => 1-element Array{Int64,1} (scalaire)
```

NE PAS PRIVILEGIER LES OPERATIONS SUR MATRICES 

=> utiliser des boucles comme en C ou du "broadcasting"

### broadcasting

utilise le dot '`.`'  => expl: `a.*b`    (à la matlab)

```
a = [1,2,3]
sin.(a)          # calcule le sinus de chaque comp 
```


### packages

```julia
using Pkg
Pkg.add("SpecialFunctions")  # installe le package


using SpecialFunctions
gamma(3)
SpecialFunctions.gamma(3)     # idem
import SpecialFunctions       # n'importe pas dans le namespace global
```

définition d'un module:

```julia
module MyModule
export func2             # func2 sera accessible via using .MyModule

a=42
function func1(x)        # func1 est accessible via MyModule.func1
    return x^2
end

function func2(x)       
    return func1(x) + a
end

end #end of module
```


utilisation:

```julia
include("mymodule.jl")
using .MyModule
...
```

### types

```julia
abstract type Person   # type abstrait
end

abstract type Musician <: Person    # "sub-type"
end

mutable struct Rockstar <: Musician   # type concret non const (mutable)
	name::String                       # "nom de variable::type"
	instrument::String
	bandName::String
	headbandColor::String
	instrumentsPlayed::Int
end

struct ClassicMusician <: Musician   # type concret "const"
	name::String
	instrument::String
end

mutable struct Physicist <: Person
	name::String
	sleepHours::Float64
	favouriteLanguage::String
end

aure = Physicist("Aurelio", 6, "Julia")
aure.sleepHours = 8
```


* multiple dispatch: permet de faire des appels à des fcts virtuelles et avoir une sorte de polymorphisme:

```julia
function introduceMe(person::Person)   # <= fct appelée pour toutes les personnes
    println("Hello, my name is $(person.name).")
end

function introduceMe(person::Musician)   # <= fct appelée pour les musiciens
    println("Hello, my name is $(person.name) and I play $(person.instrument).")
end
```

* constructeur

```julia
mutable struct MyData
	x::Float64
	x2::Float64
	y::Float64
	z::Float64
	function MyData(x::Float64, y::Float64)
		x2=x^2
		z = sin(x2+y)
		new(x, x2, y, z)  # <= crée l'instance
	end
end
```

* types parametriques

```julia
mutable struct MyData2{T<:Real}
	x::T
	x2::T
	y::T
	z::Float64
	function MyData2{T}(x::T, y::T) where {T<:Real}
		x2=x^2
		z = sin(x2+y)
		new(x, x2, y, z)
	end
end

MyData2{Float64}(2.0,3.0)
MyData2{Int}(2,3)
```

### packages

```julia
]                           # passe en mode package manager
generate("TestPackage1")    # genere qq fichiers de démarrage
```

