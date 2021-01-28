#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import sympy
import numpy as np

ksi = sympy.var('ksi')
eta = sympy.var('eta')
phi = [ 1.0 - ksi - eta, ksi, eta ]

dphi_x = [ -1.0, 1.0, 0.0 ]


M = sympy.zeros(3, 3)
for i in range(3):
    for j in range(3):
        M[i, j] = sympy.Integral(   phi[i]*phi[j], (eta, 0., 1.-ksi)  , (ksi, 0., 1.) )

Sx = sympy.zeros(3, 3)
for i in range(3):
    for j in range(3):
        Sx[i, j] = sympy.Integral(   phi[i]*dphi_x[j], (eta, 0., 1.-ksi)  , (ksi, 0., 1.) )

#print M
M = M.doit()
#print M
M = np.array(M).astype(np.float64)
print 'M =', M
Minv =  np.linalg.inv(M)
print 'Minv =', Minv

#print np.matmul(Minv, M) 

#print S
Sx = Sx.doit()
#print S
Sx = np.array(Sx).astype(np.float64)
print 'Sx =', Sx


fx = np.array([1.0, 1.0, 1.0])

print 'Sterm = ', np.matmul(Sx.transpose(), fx) 