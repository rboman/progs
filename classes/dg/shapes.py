#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# shape function management

import numpy as np
from quadrature import Gauss

class Shape(object):
    """ Traditional EF linear shape functions (2 nodes / 1st degree)
    """
    def __init__(self):
        self.xis = np.array( [-1.0, 1.0] ) # reduced coordinates of the "nodes"

    def eval(self, xi):
        """ evaluate shape fct at reduced coord "xi"
        """
        N1 = 0.5*(1-xi)
        N2 = 0.5*(1+xi)
        return np.array( [N1, N2] )

    def evalD(self, xi):
        """ first derivative of shape fcts
        """    
        dN1 = -0.5
        dN2 = 0.5
        return np.array( [dN1, dN2] )

    def interp(self, vs, xi):
        """ interpolate nodal values "vs" at xi
        """
        assert len(vs)==2
        N = self.eval(xi)
        return N.dot(vs)

class Lagrange(object):
    """ Traditional Lagrange shape functions (Np nodes / degree Np-1)
        (slow - not optimised at all)
    """
    def __init__(self, Np, classic=True):
        self.Np = Np
        # reduced coordinates of the "nodes"
        if classic:
            self.xis = np.linspace(-1.0, 1.0, Np) # classical FE positions
        else:
            gauss = Gauss(Np)
            self.xis = gauss.xg                   # DG nodal basis (leads to a diagonal mass matrix)

    def eval(self, xi):
        """ evaluate shape fct at reduced coord "xi" (brutal implementation)
        """
        if isinstance(xi, float):
            Ns = np.ones( (self.Np, 1) )
        else:
            Ns = np.ones( (self.Np, xi.shape[0]) )
        for i in range(self.Np):
            for j in range(self.Np):
                if i!=j:
                    Ns[i,:] *= (self.xis[j]-xi)/(self.xis[j]-self.xis[i])
        if isinstance(xi, float):
            Ns.shape = (self.Np, )
        
        return Ns

    def evalD(self, xi):
        """ first derivative of shape fcts (brutal implementation)
        """    
        if isinstance(xi, float):
            Ns = np.zeros( (self.Np, 1) )
        else:
            Ns = np.zeros( (self.Np, xi.shape[0]) )
        for i in range(self.Np): # we derive shp #i wrt to xi
            #sm = 0.0
            for k in range(self.Np): # sum of k terms
                prod = np.ones(xi.shape)
                if i!=k:
                    for j in range(self.Np):
                        if i!=j:
                            if j!=k:
                                prod *= (self.xis[j]-xi)/(self.xis[j]-self.xis[i])
                            else:
                                prod *= (-np.ones(xi.shape))/(self.xis[j]-self.xis[i])
                    Ns[i,:] += prod
        if isinstance(xi, float):
            Ns.shape = (self.Np, )
        return Ns

    def interp(self, vs, xi):
        """ interpolate nodal values "vs" at xi
        """
        #assert len(vs)==2
        N = self.eval(xi)
        return N.dot(vs)


if __name__=="__main__":

    # test - display Lagrange fct and derivatives of a given order
    order = 2
    shp = Lagrange(order)

    x = np.linspace(-1.0, 1.0, 100)
    s = shp.eval(np.array( x ))
    sn = shp.eval(shp.xis)
    ds = shp.evalD(np.array( x ))
    dsn = shp.evalD(shp.xis)

    # display
    import matplotlib.pyplot as plt
    plt.figure(1)
    #plt.clf()
    plt.plot(x, np.transpose(s)) #, 'ro-', markersize=4)
    plt.gca().set_prop_cycle(None)
    plt.plot(shp.xis, np.transpose(sn), 'o', markersize=8)
    
    plt.xlabel('t')
    plt.ylabel('N_i(x)')
    plt.grid(True)
    plt.title('Shape functions of order %d' % (shp.Np-1)) 


    plt.figure(2)
    #plt.clf()
    plt.plot(x, np.transpose(ds)) #, 'ro-', markersize=4)
    plt.gca().set_prop_cycle(None)
    plt.plot(shp.xis, np.transpose(dsn), 'o', markersize=8)
    
    plt.xlabel('t')
    plt.ylabel('dN_i/dx(x)')
    plt.grid(True)
    plt.title('Derivatives of shape functions of order %d' % (shp.Np-1)) 

    plt.show()    
