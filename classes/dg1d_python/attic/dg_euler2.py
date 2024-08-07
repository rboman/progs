#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# basic Discontinuous-Galerkin FEM code

import numpy as np
import numpy.linalg
import matplotlib.pyplot as plt


def shapef(xi):
    N1 = 0.5*(1-xi)
    N2 = 0.5*(1+xi)
    return N1, N2


def dshapef(xi):
    dN1 = -0.5
    dN2 = 0.5
    return dN1, dN2


def jaco(x, xi):
    dN = dshapef(xi)
    dj = 0.
    for i in range(len(x)):
        dj += dN[i]*x[i]
    return dj


def mass(x):
    xg = [-1./np.sqrt(3), 1./np.sqrt(3)]
    pg = [1., 1.]

    M = np.zeros((2, 2))
    for i in range(2):
        for j in range(2):
            v = 0.
            for n in range(2):
                s = shapef(xg[n])
                dj = jaco(x, xg[n])
                v += s[i]*s[j] * dj * pg[n]
            M[i, j] = v
    return M


def stiff(x):
    xg = [-1./np.sqrt(3), 1./np.sqrt(3)]
    pg = [1./2, 1./2]

    S = np.zeros((2, 2))
    for i in range(2):
        for j in range(2):
            v = 0.
            for n in range(2):
                s = shapef(xg[n])
                ds = dshapef(xg[n])
                v += s[i]*ds[j] * pg[n]
            S[i, j] = v
    return S


def plotfig(xe, ue2, t):
    """ plots the solution with matplotlib
    """
    plt.figure(1)
    plt.clf()
    plt.plot(xe, ue2, 'ko-', markersize=4, markerfacecolor=[1, 0, 0])
    plt.xlabel('x')
    plt.ylabel('u')
    plt.ylim(-1.2, 1.2)
    plt.grid(True)
    plt.title('t = %f' % t)
    plt.draw()
    plt.pause(.001)


def main():
    L = 10.
    ne = 10
    a = 1.
    cfl = 1./10.
    Dmax = L/2.0
    tmax = Dmax/a

    x = np.linspace(0., L, ne+1)
    dx = x[1]-x[0]

    # element => nodes
    el2nod = np.zeros((ne, 2), dtype=int)
    for i in xrange(el2nod.shape[0]):
        el2nod[i, 0] = i
        el2nod[i, 1] = i+1

    #print ex

    # build edge=>elements matrix
    edg2el = np.ones((ne+1, 2), dtype=int)*(-1)
    for i in range(ne):
        for j in range(2):
            v = el2nod[i, j]
            if edg2el[v, 0] == -1:
                edg2el[v, 0] = i
            else:
                edg2el[v, 1] = i

    xx = x[el2nod[0, :]]
    print 'xx =', xx
    M = mass(xx)
    print 'M =', M
    Minv = np.linalg.inv(M)
    print 'Minv =', Minv
    S = stiff(xx)
    print 'S =', S

    xe = np.zeros((2, ne))
    for i in range(ne):
        xe[:, i] = x[el2nod[i, :]]

    # initial condition
    ue1 = np.zeros((2, ne))
    for i in range(ne):
        ue1[:, i] = np.sin(2*np.pi*xe[:, i]/L)

    ue2 = ue1.copy()
    dt = cfl*dx/a
    t = 0.
    plotfig(xe, ue2, t)

    # tmax=100*dt
    print 'dt =', dt, ' tmax =', tmax
    while t+dt < tmax:
        t += dt

        # compute fluxes (loop over edges)
        flux = np.zeros(ne+1)
        for ied in range(ne+1):
            e1 = ied-1
            e2 = ied
            if e1 >= 0:
                u1 = ue1[1, e1]  # u val at node#2 of first el
            else:
                u1 = -np.sin(2*np.pi*t/L)  # BC (inlet)
            if e2 < ne:
                u2 = ue1[0, e2]
            else:
                u2 = u1  # BC outlet

            alpha = 0.0
            flux[ied] = a*(u1+u2)/2. + np.abs(a) * (1.-alpha)/2. * (u1-u2)
        #print 'flux=', flux

        # update DG elements (could be done in parallel)
        for iel in range(ne):
            i1, i2 = el2nod[iel]
            fe = [(-1.0) * (a*ue1[0, iel]-flux[i1]), a*ue1[1, iel]-flux[i2]]
            ue2[:, iel] = ue1[:, iel] + dt * \
                Minv.dot(-a*S.dot(ue1[:, iel]) + fe)

        plotfig(xe, ue2, t)

        ue2, ue1 = ue1, ue2


if __name__ == "__main__":
    main()
    # plt.show()
    raw_input('<ENTER TO QUIT>')
