#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Copyright (C) 2013 FlowKit Ltd, Lausanne, Switzerland
# E-mail contact: contact@flowkit.com
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License, either
# version 3 of the License, or (at your option) any later version.

"""
2D flow around a cylinder
from
https://palabos.unige.ch/get-started/lattice-boltzmann/lattice-boltzmann-sample-codes-various-other-programming-languages/
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm

###### Flow definition #########################################################
maxIter = 200000  # Total number of time iterations.
Re = 220.0  # Reynolds number.
nx = 520
ny = 180
ly = ny-1.0
q = 9  # Lattice dimensions and populations.
cx = nx/4
cy = ny/2
r = ny/9          # Coordinates of the cylinder.
uLB = 0.04                       # Velocity in lattice units.
nulb = uLB*r/Re
omega = 1.0 / (3.*nulb+0.5)  # Relaxation parameter.

print(f'uLB   = {uLB}')
print(f'nulb  = {nulb}')  # nulb  = 0.0036363636363636364
print(f'omega = {omega}') # omega = 1.9572953736654806

###### Lattice Constants #######################################################
# Lattice velocities.
c = np.array([(x, y) for x in [0, -1, 1] for y in [0, -1, 1]])
# c =
# [[ 0  0]
#  [ 0 -1]       5 \ 2 / 8
#  [ 0  1]       3 - 0 - 6
#  [-1  0]       4 / 1 \ 7
#  [-1 -1]
#  [-1  1]
#  [ 1  0]
#  [ 1 -1]
#  [ 1  1]]

# Lattice weights.
t = 1./36. * np.ones(q)                                   
t[np.asarray([np.linalg.norm(ci) < 1.1 for ci in c])] = 1./9.
t[0] = 4./9.
# t = [0.44444444 0.11111111 0.11111111 0.11111111 0.02777778 0.02777778
#      0.11111111 0.02777778 0.02777778]

# inconnues opposées à [0, 1, 2, 3, 4, 5, 6, 7, 8]
noslip = [c.tolist().index((-c[i]).tolist()) for i in range(q)]
#          [0, 1, 2, 3, 4, 5, 6, 7, 8]
# noslip = [0, 2, 1, 6, 8, 7, 3, 5, 4]
i1 = np.arange(q)[np.asarray([ci[0] < 0 for ci in c])]  # Unknown on right wall.
i2 = np.arange(q)[np.asarray([ci[0] == 0 for ci in c])]  # Vertical middle.
i3 = np.arange(q)[np.asarray([ci[0] > 0 for ci in c])]  # Unknown on left wall.
# i1 = [3 4 5]
# i2 = [0 1 2]   (OK, ...correspond au schéma des vitesses)
# i3 = [6 7 8]

###### Function Definitions ####################################################

# Helper function for density computation.
def sumpop(fin): 
    return np.sum(fin, axis=0)

# Equilibrium distribution function.
def equilibrium(rho, u):
    """compute feq from velocity "u" and density "rho"
    rho: scalar or (nx,ny)
    shape(u) = (2,nx,ny)
    """             
    # note: dot(a, b)[i,j,k,m] = sum(a[i,j,:] * b[k,:,m])
    cu = 3.0 * np.dot(c, u.transpose(1, 0, 2))   # = sum( c[i,:] * u[:,j,k])  [shape=(q,nx,ny)]
    usqr = 3./2.*(u[0]**2+u[1]**2)
    feq = np.zeros((q, nx, ny))
    for i in range(q):
        feq[i, :, :] = rho*t[i]*(1.+cu[i]+0.5*cu[i]**2-usqr)
    return feq


###### Setup: cylindrical obstacle and velocity inlet with perturbation ########
# table of bool (obstacle[i,j] == True if (i,j) inside the obstacle)
obstacle = np.fromfunction(lambda x, y: (x-cx)**2+(y-cy)**2 < r**2, (nx, ny))
# v_x(i,j) = vel[0,i,j]; v_y(i,j) = vel[1,i,j]
vel = np.fromfunction(lambda d, x, y: (1-d)*uLB *
                   (1.0+1e-4*np.sin(y/ly*2*np.pi)), (2, nx, ny))
feq = equilibrium(1.0, vel)  # rho=1.0 (this is the initial density)
fin = feq.copy()

###### Main time loop ##########################################################
for time in range(maxIter):
    # Right wall: outflow condition.
    fin[i1, -1, :] = fin[i1, -2, :] 

    # Calculate macroscopic density and velocity.
    rho = sumpop(fin)           
    u = np.dot(c.transpose(), fin.transpose((1, 0, 2)))/rho    # u[i,j,k] = sum( c[:,i] * fin[:,j,k])/rho   [shape=(2,nx,ny)]

    # Left wall: compute density from known populations.
    u[:, 0, :] = vel[:, 0, :]
    rho[0, :] = 1./(1.-u[0, 0, :]) * (sumpop(fin[i2, 0, :]) + 2.*sumpop(fin[i1, 0, :]))

    # Left wall: Zou/He boundary condition.
    feq = equilibrium(rho, u)  
    fin[i3, 0, :] = fin[i1, 0, :] + feq[i3, 0, :] - fin[i1, 0, :]  # =feq[i3, 0, :]  ????BUG????

    # Collision step.
    fout = fin - omega * (fin - feq)  

    for i in range(q):
        fout[i, obstacle] = fin[noslip[i], obstacle]

    # Streaming step.
    for i in range(q):  
        fin[i, :, :] = np.roll(
            np.roll(fout[i, :, :], c[i, 0], axis=0), c[i, 1], axis=1)

    # Visualization
    freq = 10
    if (time % freq == 0): 
        plt.clf()

        # vorticity = (np.roll(u[0], -1, axis=0) - np.roll(u[0], 1, axis=0)) - \
        #     (np.roll(u[1], -1, axis=1) - np.roll(u[1], 1, axis=1))  
        # vorticity[obstacle] = np.nan
        # cmap = plt.cm.bwr
        # im = plt.imshow(vorticity.T, cmap='bwr')
        # plt.clim(-.01, .01)

        im = plt.imshow(np.sqrt(u[0]**2+u[1]**2).transpose(), cmap=cm.Reds)

        # im = plt.imshow(rho.transpose(), cmap=cm.Reds)  # ~1
        # legend-begin
        from mpl_toolkits.axes_grid1 import make_axes_locatable
        divider = make_axes_locatable(plt.gca())
        cax = divider.append_axes("right", size="5%", pad=0.05)
        plt.colorbar(im, cax=cax)
        # legend-end

        # plt.savefig("vel."+str(int(time/freq)).zfill(5)+".png")
        plt.pause(0.001)
