#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# matrices used in the DG formaulation

import numpy as np
#import numpy.linalg


class Mass(object):
    """  M = \int N_i.N_j dV
    """
    def __init__(self):
        pass
    def build(self, el, x):
        """ calculate the M matrix of element "el" 
            "x" is the vector of coordinates (used by dj)
        """
        gauss = el.gauss
        shpu = el.shpu

        M = np.zeros((el.nnod, el.nnod))
        for i in range(el.nnod):
            for j in range(el.nnod):
                v = 0.
                for n in range(gauss.npg):
                    s = shpu.eval(gauss.xg[n])
                    dj = el.jaco(x, gauss.xg[n])
                    v += s[i]*s[j] * dj * gauss.pg[n]
                M[i, j] = v
        return M   


class Stiff(object):
    """  S = \int N_i.dN_j dV
    """
    def __init__(self):
        pass
    def build(self, el, x):
        """ calculate the S matrix of element "el" 
            "x" is the vector of coordinates (useless since dj/dj=1)
        """
        gauss = el.gauss
        shpu = el.shpu

        S = np.zeros((el.nnod, el.nnod))
        for i in range(el.nnod):
            for j in range(el.nnod):
                v = 0.
                for n in range(gauss.npg):
                    s = shpu.eval(gauss.xg[n])
                    ds = shpu.evalD(gauss.xg[n])
                    v += s[i]*ds[j] * gauss.pg[n]
                S[i, j] = v
        return S
