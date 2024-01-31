#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# basic test

import numpy as np

from elements import Mesh
from timeint import BwEuler, RK2, RK4
from dg import DG, RHS1
import time, sys
from shapes import Lagrange

def main():

    L = 10.         # length of the domain
    ne = 3          # number of elements
    order = 6       # order = degree+1 (see Zanotti p44)

    shpu = Lagrange(order, classic=False)
    msh = Mesh(L, ne, shpu)

    a = 3.0                         # velocity
    cfl = 1.0/(2*(order-1)+1)       # stability limit (cfr. slides Zanotti p29)
    cfl = cfl/2.                    # security
    # the cfl should use the smallest distance between 2 nodes within an element (see Hesthaven p66) 

    Dmax = L #L/2.0                 # total distance
    tmax = Dmax/np.abs(a)
    #tmax = t+10*dt
    alpha = 0.0  # upwind coefficient (alpha=0 => full upwind)

    # initial/boundary conditions

    # .sinus 
    def u0(x): return np.sin(2*np.pi*x/L*2)
    def inletf(t): return -np.sin(2*np.pi*a*t/L*2)
    def fexact(x, t): return np.sin(2*np.pi*(x-a*t)/L*2)
    # .step
    #u0 = lambda x : np.zeros(x.shape)
    #inletf = lambda t : 1.0

    # calculate time step from prescribed CFL
    dx = msh.x[1]-msh.x[0]
    dt = cfl*dx/np.abs(a)
    print 'dt =', dt, ' tmax =', tmax
    
    rhs1 = RHS1(inletf, a, alpha, msh)
    tinteg = RK4(rhs1)
    #tinteg = BwEuler(rhs1)
    dg = DG(msh, dt, tinteg, u0, tmax, fexact)
    #dg.nogui = True
    dg.run()




if __name__ == "__main__":
    main()
    # plt.show()
    raw_input('<ENTER TO QUIT>')