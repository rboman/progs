#!/usr/bin/python

# my numpy sandbox
# doc: https://numpy.org/doc/stable/index.html


import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm

# some tests with a 2D array / axis / sum
# ---------------------------------------
A = np.array([[1, 2], [3, 4], [5, 6]])
print(f'A = {A}')
print(f'A[0,1] = {A[0,1]}')
print(f'A.shape = {A.shape}')
# somme les lignes (1er indice = 0)
print(f'np.sum(A, axis=0) = {np.sum(A, axis=0)}')
# somme les colonnes (2e indice = 1)
print(f'np.sum(A, axis=1) = {np.sum(A, axis=1)}')

# some tests with a 3D array
# --------------------------
B = np.array([[[1, 2, 3, 4], [5, 6, 7, 8]],
              [[11, 12, 13, 14], [15, 16, 17, 18]],
              [[21, 22, 23, 24], [25, 26, 27, 28]] ])

print(f'B = {B}')
print(f'B[0,1,2] = {B[0,1,2]}') # 7
print(f'B.shape = {B.shape}')  # (3, 2, 4)
# on somme suivant l'axe 0 => on récupère une matrice de taille 2,2
print(f'np.sum(B, axis=0) = {np.sum(B, axis=0)}')

# ordering
B_flat = np.reshape(B, newshape=(-1,)) # order='C' is the default / negative size = "auto"
print(f'B_flat = {B_flat}')
B_flat = np.reshape(B, newshape=(-1,), order='F') # 'F' means Fortran
print(f'B_flat = {B_flat}')

# transposed
print(f'B.T = {B.T}')  # (i,j,k) => (k,i,j)
print(f'B.T.shape = {B.T.shape}')
print(f'B[2,1,0] = {B.T[2,1,0]}') # 7
print(f'np.transpose(B, axes=(1,0,2)) = {np.transpose(B, axes=(1,0,2))}')  # (i,j,k) => (j,i,k)

