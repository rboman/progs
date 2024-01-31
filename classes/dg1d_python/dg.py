#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# basic Discontinuous-Galerkin FEM code

import numpy as np
import numpy.linalg
#import matplotlib.pyplot as plt

from display import Display
from matrices import Mass, Stiff
from shapes import Shape
from quadrature import Gauss
from elements import Element, Mesh
from timeint import BwEuler, RK4, RHS
import time, sys

class RHS1(RHS):
    def __init__(self, inletf, a, alpha, msh):
        # store arguments
        self.inletf = inletf
        self.a = a
        self.alpha = alpha
        self.msh = msh

        # compute M, Minv and S matrices (same for all elements)
        m = Mass()
        M = m.build(msh.elms[0], msh.x)
        self.Minv = np.linalg.inv(M)
        s = Stiff()
        self.S = s.build(msh.elms[0], msh.x)

        if 0:
            print 'M =', M
            print 'Minv =', self.Minv
            print 'S =', self.S
            #sys.exit()

    def __call__(self, u, t):
        """ see RHS, TimeInt interface
        """
        return self.computerhs(u, self.inletf, t, self.a, self.alpha, self.msh, self.Minv, self.S)   


    def computeF(self, ue1, inletf, t, a, alpha, msh):
        """ compute numerical flux
        """
        flux = np.zeros(len(msh.edges)) # create a new vector
        for ed in msh.edges.itervalues():
            e1 = ed.elms[0]   # 1= interior element (the normal points away from it)
            u1 = e1.evalu(ed, ue1)

            if len(ed.elms)>1:
                e2 = ed.elms[1]  # 2=exterior element (do not exist on a boundary)
                u2 = e2.evalu(ed, ue1)
            else:
                e2 = None
                #print 'ed.nor=', ed.nor
                if ed.nor*a<0.0: # inlet
                    u2 = inletf(t)
                else:
                    u2 = u1  # BC outlet
            
            if a*ed.nor>0.0:
                fstar = a*(u1+u2)/2. - (1.-alpha)/2. * a*(u2-u1)  # oriented as the normal
            else:
                fstar = a*(u1+u2)/2. + (1.-alpha)/2. * a*(u2-u1) 
            
            #print 'edge', ed.idx, 'u1=', u1, 'u2=', u2, 'flux=', fstar
            flux[ed.idx] = fstar

        return flux


    def computerhs1e(self, ue1, el, flux, a, Minv, S):
        """ compute the right-hand side for 1 element
        """
        # dofs of the element
        u = ue1[el.idx:el.idx+el.nnod]
        #print 'u=', u

        # "a.u" flux on the boundary
        auvert = [a*el.shpu.interp(u, xi=-1.0), a*el.shpu.interp(u, xi=1.0)]
        #print 'auvert=',auvert

        # numerical flux on the boundary
        fstar = flux[el.verts]
        #print 'fstar=',fstar

        nor = [ el.edges[0].nor, el.edges[1].nor ]  # edges normals
        #print 'nor=',nor

        fe = np.zeros(u.shape)
        N0 = el.shpu.eval(xi=-1.0) # eval all shape fcts on the edges
        N1 = el.shpu.eval(xi=+1.0)

        for i in range(el.nnod): # TODO remove el.nnod => shpu.Np
            fe[i] = (auvert[1]-fstar[1])*N1[i] - (auvert[0]-fstar[0])*N0[i]

        rhse = Minv.dot(-a*S.dot(u) + fe)
        #print 'rhse=', rhse
        return rhse


    def computerhs(self, ue1, inletf, t, a, alpha, msh, Minv, S):
        """ compute the right hand side for all elements
        """
        # compute all edge fluxes (loop over all edges)
        flux = self.computeF(ue1, inletf, t, a, alpha, msh)
        #print 'flux=',flux

        # update DG elements (could be done in parallel)
        #ne = ue1.shape[1]
        rhs = np.zeros(msh.nu)
        for el in msh.elms:
            rhs[el.idx:el.idx+el.nnod] = self.computerhs1e(ue1, el, flux, a, Minv, S)
        #print 'rhs=', rhs
        return rhs


class DG(object):
    def __init__(self, msh, dt, tinteg, u0, tmax, fexact):
        self.msh = msh 
        self.dt = dt 
        self.tinteg = tinteg 
        self.u0 = u0 
        self.tmax = tmax 
        self.nogui = False
        self.fexact = fexact
        
    def run(self):
        # fill positions of the nodes of elems => xnod
        # xnod is used for the GUI and for eval of "u0"
        xnod = np.zeros(self.msh.nu)
        for e in self.msh.elms:
            e.setux(self.msh.x, xnod)

        # initial condition
        ue1 = self.u0(xnod)
        t = 0.
        if not self.nogui:
            gui = Display(self.msh, self.fexact)
        #nstep=1
        cpu1 = time.clock()
        while 1:
            # display solution
            if not self.nogui:
                gui.update(xnod, ue1, t)  
            # check the end of the simulation      
            if t > self.tmax-self.dt/1000.:
                break
            #print '** time step #%d (t=%e) *********************************' % (nstep, t+dt)
            ue2 = self.tinteg.onestep(ue1, t, self.dt)
            t += self.dt
            #nstep += 1
            ue1 = ue2
        cpu2 = time.clock()
        print 'elapsed CPU time=', cpu2-cpu1

