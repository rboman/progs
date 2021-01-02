# julia

pas de julia dans les packages Ubuntu 18.04 => telecharger les binaires linux

gros changements entre version 0.x et 1.x (2018!)

## Liens

* https://techytok.com/from-zero-to-julia/

* https://syl1.gitbook.io/julia-language-a-concise-tutorial/

* https://benlauwens.github.io/ThinkJulia.jl/latest/book.html


## Memo

quitter: exit() ou CTRL-D

beaucoup de choses comme MATLAB:
- ";" en interactif supprime l'output
- les index démarrent à 1!
- slices: a[2:3]

par contre:
- les variables sont des pointeurs comme en python (a=b, a[2]=... modifie b) 
- pas besoin de coder avec des operations matricielles => utiliser des boucles "for i,j" est OK!

import Pkg; Pkg.add("Plots") # telecharge une chiée de packages dont Plots
using Plots                  # précompile Plots la 1ere fois (long!)
import Pkg; Pkg.add("PyPlot") 
pyplot()                      # => installe matplotlib, qt, pyqt   et passe en backend pyplot pour Plots

plot(rand(4,4))

?   => passe en mode "help"

### types

a = 2.0
typeof(a)    => Float64
convert(Int64, a)    # => 2

### functions

"""
documentation de f
"""
function f(x)
    return 2*x*x
end

f(x) = 2*x*x   # version courte
f(x) = 2x^2    # version très courte!

## arrays

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


## tuples

similaires à python

a = 1,2
a = (1,2)        # idem
print("a=$a")    # impression

## dictionnaires

person1 = Dict("Name" => "Aurelio", "Phone" => 123456789, "Shoe-size" => 40)

## control flow

très similaire à python mis à part la présence de "end" pour delimiter les blocs

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


enumerate() # idem à python (retourne un tuple)

## arrays operations

a = [1,2,3]  # column vector
b = [4 5 6]  # row vector

a*b => 3×3 Array{Int64,2}
b*a => 1-element Array{Int64,1} (scalaire)

NE PAS PRIVILEGIER LES OPERATIONS SUR MATRICES 
 => utiliser des boucles comme en C ou du "broadcasting"

## broadcasting

utilise le dot '.'  => expl: a.*b    (à la matlab)

a = [1,2,3]
sin.(a)          # calcule le sinus de chaque comp 


