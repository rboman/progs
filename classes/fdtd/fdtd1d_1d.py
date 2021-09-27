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

class FDTD:
    
    def __init__(self):
        
        self.imp0 = 377.0     # impedance
        
        self.SIZE = 200       # nb of spatial grid points  
        self.maxTime = 450    # nb of time steps
        self.nloop = 1        # nb of loops / refresh of the display

        self.MAT_LAYER = 100
        self.LOSS_LAYER = 180  # position of the lossy layer
        self.LOSS = 0.02

        self.EPSR = 9.0

        self.TFSF = 50 # position of the TFSF boundary

        self.Cdtds = 1.0 # Courant
        self.width = 10. # width of the Gaussian input
        self.delay = 30. # delay of the Gaussian input

    def execute(self):
        
        # if not args.nogui:
        self.fig, self.ax = plt.subplots() #nrows=2)
        # else:
        #     self.fig = self.ax = None

        self.qTime = 0

        self.ez = np.zeros(self.SIZE)
        self.hy = np.zeros(self.SIZE-1)

        # E eq
        self.ceze = np.zeros(self.SIZE)
        self.cezh = np.zeros(self.SIZE)

        self.ceze[:self.MAT_LAYER] = 1.0
        self.ceze[self.MAT_LAYER:self.LOSS_LAYER] = 1.0
        self.ceze[self.LOSS_LAYER:] = (1.0-self.LOSS) / (1.0+self.LOSS)

        self.cezh[:self.MAT_LAYER] = self.imp0
        self.cezh[self.MAT_LAYER:self.LOSS_LAYER] = self.imp0 / self.EPSR
        self.cezh[self.LOSS_LAYER:] = self.imp0 / self.EPSR / (1.0+self.LOSS)

        # H eq
        self.chyh = np.zeros(self.SIZE-1)
        self.chye = np.zeros(self.SIZE-1)

        self.chyh[:self.LOSS_LAYER] = 1.0
        self.chyh[self.LOSS_LAYER:] = (1.0-self.LOSS) / (1.0+self.LOSS)

        self.chye[:self.LOSS_LAYER] = 1.0 / self.imp0
        self.chye[self.LOSS_LAYER:] = 1.0 / self.imp0 / (1.0+self.LOSS)
    
        # initialise matplotlib

        xe = np.linspace(0.0, self.SIZE-1, self.SIZE)
        xh = np.linspace(0.0, self.SIZE-2, self.SIZE-1) + 0.5
        
        # if not args.nogui:
        self.line1, = plt.plot(xe, self.ez, label=r'$E_z$')
        self.line2, = plt.plot(xh, self.hy*self.imp0, label=r'$H_y \eta_0$')
        plt.title('---')
        plt.xlabel(r'$X$')
        plt.ylabel(r'$E_z & H_y\eta_0$')
        #plt.ylim(-2,2)
        plt.ylim(-1.1, 1.5)
        plt.plot([self.MAT_LAYER, self.MAT_LAYER], plt.ylim(), 'r--', linewidth=2.0)
        plt.plot([self.LOSS_LAYER, self.LOSS_LAYER], plt.ylim(), 'b--', linewidth=2.0)
        plt.grid(True)
        plt.legend(loc='upper left')

        plt.draw()

        ani = animation.FuncAnimation(self.fig, self.animate, (self.maxTime-1) // self.nloop, 
                                            interval=25, repeat=False)
        plt.show()
        # else:
        #     # calls the loops without matplotlib
        #     for i in range((self.maxTime-1) // self.nloop):
        #         self.animate(i)           

    def animate(self, i):
        for l in range(self.nloop):
            self.qTime = self.qTime + 1

            self.updateH()
            self.updateTFSF()
            self.abc()
            self.updateE()

            # additive source
            #ez[50] += np.exp(-(qTime-30.)*(qTime-30.)/100.)
        # if not args.nogui:
        self.line1.set_ydata(self.ez)
        self.line2.set_ydata(self.hy*self.imp0)
        self.ax.set_title('time = %d' % self.qTime)  
        return self.line1, self.line2

    def abc(self):
        """absorbing boundary condition"""
        self.ez[0] = self.ez[1] 
        #self.ez[self.SIZE-1] = self.ez[self.SIZE-2]

    def updateH(self):
        for mm in range(0, self.SIZE-1):
            self.hy[mm] = self.chyh[mm] * self.hy[mm] + self.chye[mm] * (self.ez[mm+1] - self.ez[mm])

    def updateE(self):
        for mm in range(1, self.SIZE-1):
            self.ez[mm] = self.ceze[mm] * self.ez[mm] + self.cezh[mm] * (self.hy[mm] - self.hy[mm-1])

    def ezInc(self, time, location):
        return np.exp(-math.pow((time - self.delay - location/self.Cdtds) / self.width, 2))
    
    def updateTFSF(self):
        # correction for TFSF boundary
        self.hy[self.TFSF] -= self.ezInc(self.qTime, 0.0) * self.chye[self.TFSF]           
        self.ez[self.TFSF+1] += self.ezInc(self.qTime+0.5, -0.5) 
        

if __name__ == "__main__":
    sim = FDTD()
    sim.execute()
