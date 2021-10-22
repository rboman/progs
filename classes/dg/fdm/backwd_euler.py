#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

def main():
    L = 10.
    ne = 30
    a = 1.
    cfl = 0.5
    Dmax = L/2.
    tmax = Dmax/a

    x = np.linspace(0., L, ne+1)
    dx = x[1]-x[0]
    u1 = np.zeros(len(x))
    u2 = np.zeros(len(x))

    line, = plt.plot(x,u2,'o-')
    plt.xlabel('x')
    plt.ylabel('u')
    plt.ylim(-0.2,1.2)
    plt.grid(True)
    plt.draw()

    t = 0
    dt = cfl*dx/a
    while t<tmax:
        t+=dt
        u2[0] = 1. # BC
        for i in xrange(1,len(x)):
            u2[i] = u1[i] - cfl * (u1[i]-u1[i-1])
        
        line.set_ydata(u2)
        plt.title('t = %f' % t)
        plt.draw()
        plt.pause(0.001)
        u1,u2 = u2,u1


if __name__ == "__main__":
    main()
    plt.show()
    #raw_input('<ENTER TO QUIT>')
