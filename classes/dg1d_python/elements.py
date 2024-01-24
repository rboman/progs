#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# mesh-related objects

import numpy as np
from shapes import Shape, Lagrange
from quadrature import Gauss


class Element(object):
    def __init__(self, nv1, nv2, gauss, shpu):
        self.verts = [nv1, nv2]     # indexes of the 2 vertices
        self.shpx = Shape()         # geometric shape fct (linear in any case) TODO: make it static
        self.edges = []             # 2 edges
        
        # additional data (see "setfield" method) 
        self.idx = -1       # index in vectors
        self.gauss = None   # quadrature rule for unknown integration
        self.shpu = None    # interpolation of the unknown (could be of higer order)
        self.nnod = -1      # nb of nodes where the unknown is calculated (could be deduced from above vars)

    def setfield(self, shpu, gauss, idx):
        self.idx = idx   # store elm index 
        self.shpu = shpu # interp of the unknown (could be of higher order)
        self.gauss = gauss
        self.nnod = len(self.shpu.xis) 
        #assert gauss.npg==len(self.shpu.xis) # no we may use many gps
        return idx+self.nnod   # return next idx

    def jaco(self, x, xi):
        xv = x[self.verts]
        dN = self.shpx.evalD(xi)
        dj = 0.
        for i in range(len(xv)):
            dj += dN[i]*xv[i]
        return dj

    def setux(self, x, target):
        """ interpolate "x" at position of nodes into "target" vector
        """
        # get pos of vertices
        xverts = x[self.verts]

        xis = self.shpu.xis    # xi's of unknowns
        idx = self.idx
        for xi in xis:
            target[idx] = self.shpx.interp(xverts, xi)
            idx+=1

    def evalu(self, ed, u):
        """ eval field "u" on the edge "ed"
        """
        if ed==self.edges[0]:
            xi = self.shpx.xis[0]   # first xi
        else:
            xi = self.shpx.xis[-1]  # last xi
        u = self.shpu.interp(u[self.idx:self.idx+self.nnod], xi)   # could be simpler in case of nodes defined on the edge
        return u

    def evalu2(self, xis, u):
        us = []
        for xi in xis:
            us.append( self.shpu.interp(u[self.idx:self.idx+self.nnod], xi) )
        return us

    def evalx2(self, xis, x):
        xs = []
        for xi in xis:
            xs.append( self.shpx.interp(x[self.verts], xi) )
        return xs
    def __str__(self):
        return "Element verts=[%d,%d]" % self.verts


class Edge(object):
    def __init__(self, idx, nor):
        self.idx = idx     # link to the related vertex (index in "x" vector)
        self.elms = []     # list of neighbours (size=1 if boundary)
        self.nor = nor     # normal: edge orientation (-1 or +1)
    def __str__(self):
        return "Edge #%d nor=%f" % (self.idx, self.nor)

class Mesh(object):
    def __init__(self, L, ne, shpu):
        # build vector of vertices => "x"
        self.L = L                          # length of the domain
        self.ne = ne                        # nb of FE
        self.x = np.linspace(0., L, ne+1)   # xpos of vertices
      
        # build elements
        gauss = Gauss(shpu.Np)
        self.elms = []
        idx = 0
        for i in xrange(ne):
            el = Element(i, i+1, shpu, gauss)
            idx = el.setfield(shpu, gauss, idx)
            self.elms.append(el)

        self.nu = idx  # total number of unknowns    

        # build all edges from vertices
        self.edges = {}                  # map indexed by vertex no

        # buid and link edges to elms
        nor = [-1.0, 1.0]
        for el in self.elms:
        #for el in reversed(self.elms):  # test
            for i,v in enumerate(el.verts):
                ed = self.edges.get(v)
                if not ed:
                    ed = Edge(v, nor[i])
                    self.edges[v] = ed
                el.edges.append(ed)   # element => edge
                ed.elms.append(el)    # edge => element

        #for ed in self.edges.itervalues():
        #    print ed
     