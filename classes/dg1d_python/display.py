#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# basic GUI using matplotlib

import numpy as np
#import numpy.linalg
import matplotlib.pyplot as plt

class Display(object):
    def __init__(self, msh, fexact):
        self.msh = msh
        self.fexact = fexact

    def update(self, xe, ue2, t):
        """ plots the solution with matplotlib
        """

        xis = np.linspace(-1.0, 1.0, 20) # 20pts per element

        xall = []
        uall = []
        for e in self.msh.elms:
            uall.extend( e.evalu2(xis, ue2) )
            xall.extend( e.evalx2(xis, self.msh.x) )
        uexact = self.fexact(np.array(xall), t)
        plt.figure(1)
        plt.clf()
        plt.plot(xall, uexact, 'r-')
        plt.plot(xall, uall, 'k-')
        plt.plot(xe, ue2, 'o', markersize=4, markerfacecolor=[1, 0, 0], markeredgecolor=[0,0,0])
        plt.plot
        plt.xlabel('x')
        plt.ylabel('u')
        plt.ylim(-1.2, 1.2)
        plt.grid(True)
        plt.title('t = %f' % t)
        plt.draw()
        plt.pause(.001)

    def show(self):
        plt.show()