#! /usr/bin/env python
# -*- coding: utf-8 -*-

# from https://docs.scipy.org/doc/numpy-dev/user/quickstart.html

from __future__ import print_function
import numpy as np
a = np.arange(15).reshape(3, 5)

print("a=",a)
print("\ta.shape =",a.shape)
print("\ta.ndim =",a.ndim)
print("\ta.dtype.name =",a.dtype.name)
print("\ta.itemsize =",a.itemsize)
print("\ta.size =",a.size)
print("\ttype(a) =",type(a))

b = np.array([6, 7, 8])
print("b=",b)
print("\ttype(b)=",type(b))
print("\tb.dtype.name =",b.dtype.name)

print(np.array([(1.5,2,3), (4,5,6)]))

print(np.zeros( (3,4) ))

print(np.ones( (2,3,4), dtype=np.int32 ))

print(np.empty( (2,3) ))

x = np.linspace( 0, 2*np.pi, 5 )
f = np.sin(x)    # np.exp, np.sqrt, nop.cos, etc
print("x=",x)
print("sin(x)=",f)

# Arithmetic operators on arrays apply elementwise

A = np.array( [[1,1], [0,1]] )
B = np.array( [[2,0], [3,4]] )
print("A=", A)
print("B=", B)
print("A*B=", A*B)                         # elementwise product
print("A.dot(B)=", A.dot(B))     # matrix product   


b = np.arange(12).reshape(3,4)
print("b=", b)
print("b.sum(axis=0) =", b.sum(axis=0))
print("b.min(axis=1) =", b.min(axis=1))
print("b.max(axis=0) =", b.max(axis=0))

# indexing / slicing

a = np.arange(10)+3
print("a =", a)
print("a[2] =", a[2])
print("a[2:4] =", a[2:4])     # renvoie une "view", c a d une ref
print("a[0:6:2] =", a[0:6:2])  #i=>j par pas de k
print("a[:] =",a[:])
print("a[-1] =",a[-1])

b = np.fromfunction(lambda x,y: 10*x+y,(5,4),dtype=int) #creation par fct
print("b =", b)
print("b[1:3, : ] =", b[1:3, : ])
print("b[2] =", b[2])  # 3ieme ligne

# iteration
for element in b.flat:
    print(element)

# shape

print("A.T = ", A.T)
a= np.arange(12)
print("a=", a)
a.reshape(3,4) # a n'est PAS modifié => renvoie un objet
print("a=", a)
a.resize(3,4) # a est modifié
print("a=", a)

# views/copies
d = a           # d n'est qu'une ref vers a (1 seul objet numpy)
d = a.view()    # d est un nouvel objet numpy qui pointe vers les mêmes données
                # les slices fonctionnent comme ca
d = a.copy()    # d est indep de "a"




