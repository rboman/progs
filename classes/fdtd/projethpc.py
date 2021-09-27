#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright 2020 University of Liège
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


import math
import numpy as np
# from fwk.wutils import parseargs

# args = parseargs()
# if not args.nogui:
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

class FDTD:
    def __init__(self):
        self.L = 300.0e-3
        self.Ls = 10.0e-3
        self.f = 2.4e9  # Hz
        self.mu = 4e-7*math.pi  # H/m
        self.eps = 8.854e-12    # F/m
 
        self.c = 1 / math.sqrt(self.mu*self.eps)
        print('c =', self.c)
        print('wavelength =', self.c / self.f)  # ~12cm pour 2.4GHz

        self.dx = 5e-3
        self.dt = self.dx/self.c/2
        #self.dt = 1.0/self.f/20  #math.sqrt(3)*self.dx * 0.5 / self.c
        print('dt =', self.dt)
        self.nstep = 50



    def run(self):
        print('running...')

        # calculate the grid size
        npx = int(math.floor(self.L/2.0/self.dx))*2+1
        print('npx =', npx)

        nps = int(math.floor(self.Ls/2.0/self.dx))*2+1 
        print('nps =', nps)

        is1 = (npx-nps) // 2

        Z = math.sqrt(self.mu / self.eps)
        print('Z =', Z)
        CFL = self.c*self.dt/self.dx
        print('CFL =', CFL)

        # init fields
        self.ez = np.zeros( (npx,npx) )
        self.hx = np.zeros( (npx,npx) )
        self.hy = np.zeros( (npx,npx) )


        X = np.arange(0, npx, 1.0)
        Y = np.arange(0, npx, 1.0)
        X, Y = np.meshgrid(X, Y)
        #print(X)

        # args = parseargs()
        # if not args.nogui:
        fig = plt.figure()
        ax = Axes3D(fig)
        msh = ax.plot_surface(X, Y, self.ez, rstride=1, cstride=1, cmap=cm.viridis)
        ax.set_zlim(-1, 1)
        ax.set_xlabel(r'$X$')
        ax.set_ylabel(r'$Y$')
        ax.set_zlabel(r'$E_z$')
        plt.draw()

        # time integration
        time = 0.0
        for i in range(self.nstep):
            time += self.dt

            for i in range(npx):
                for j in range(npx-1):
                    self.hx[i,j] += - CFL/Z * (self.ez[i,j+1] - self.ez[i,j]) 

            for i in range(npx-1):
                for j in range(npx):
                    self.hy[i,j] +=   CFL/Z * (self.ez[i+1,j] - self.ez[i,j]) 

            for i in range(1,npx):
                for j in range(1,npx):
                    self.ez[i,j] +=   Z*CFL * (self.hy[i,j] - self.hy[i-1,j]) - Z*CFL * (self.hx[i,j] - self.hx[i,j-1])
            
            # apply source
            for i in range(is1,is1+nps+1):
                for j in range(is1,is1+nps+1):
                    self.ez[i,j] = math.sin(2*math.pi*self.f*time)
                    
            # if not args.nogui:
            if msh:
                ax.collections.remove(msh) 
            msh = ax.plot_surface(X, Y, self.ez, rstride=1, cstride=1, cmap=cm.viridis)
            plt.pause(.001)

        # if not args.nogui:
        plt.show()

if __name__=="__main__":
    

    """
        X = np.arange(-5, 5, 0.25)
        Y = np.arange(-5, 5, 0.25)
        X, Y = np.meshgrid(X, Y)
        R = np.sqrt(X**2 + Y**2)
        Z = np.sin(R)

        fig = plt.figure()
        ax = Axes3D(fig)
        ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap=cm.viridis)

        plt.show()
    """
    sim = FDTD()
    sim.run()