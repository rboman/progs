#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np 
import navaro as na

vitesses = np.array([[0, 1, 2], [0, 3, 2], [0, 1, 3]], dtype=np.float64)
positions = np.array([[0, 0, 0], [0, 0, 0], [0, 0, 0]], dtype=np.float64)
print('type(pos) =', positions.dtype)
na.move(positions, vitesses, 0.1)
print('p=', positions) #le tableau n'est pas mis a jour, stockage C

positions = np.array(positions, dtype=np.int8, order='F')
print('type(pos) =', positions.dtype)
na.move(positions, vitesses, 0.1)
print('p=', positions) #le tableau n'est pas mis a jour, mauvais type!

positions = np.array(positions, dtype=np.float64, order='F')
print('type(pos) =', positions.dtype)
na.move(positions, vitesses, 0.1)
print('p=', positions) #le tableau est modifie, stockage Fortran

##########

print('A=', na.create_array(5))

########

A = np.array([[0, 1, 2], [2, 3, 4]], dtype=np.float64)
B = np.array([[2, 1], [3, 2], [1, 9]], dtype=np.float64)
C = na.mult_array(A,B)
print("C=", C)
print("C=", np.dot(A,B))

