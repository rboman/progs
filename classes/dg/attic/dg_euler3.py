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


def buildmesh(L, ne):
    x = np.linspace(0., L, ne+1)
    # element => nodes
    el2nod = np.zeros((2, ne), dtype=int)
    for i in xrange(el2nod.shape[1]):
        el2nod[0, i] = i
        el2nod[1, i] = i+1
    return x, el2nod


def computeF(ue1, inletf, t, a, alpha):
    ne = ue1.shape[1]
    flux = np.zeros(ne+1)
    for ied in range(ne+1):
        e1 = ied-1
        e2 = ied
        if e1 >= 0:
            u1 = ue1[1, e1]  # u val at node#2 of first el
        else:
            u1 = inletf(t)  # BC (inlet)
        if e2 < ne:
            u2 = ue1[0, e2]
        else:
            u2 = u1  # BC outlet

        flux[ied] = a*(u1+u2)/2. + np.abs(a) * (1.-alpha)/2. * (u1-u2)
    return flux


def computerhs1e(iel, ue1, el2nod, flux, a, Minv, S):
    i1, i2 = el2nod[:, iel]
    fe = [(-1.0) * (a*ue1[0, iel]-flux[i1]), a*ue1[1, iel]-flux[i2]]
    rhs = Minv.dot(-a*S.dot(ue1[:, iel]) + fe)
    return rhs


def computerhs(ue1, inletf, t, a, alpha, el2nod, Minv, S):
    # compute all edge fluxes (loop over all edges)
    flux = computeF(ue1, inletf, t, a, alpha)
    # update DG elements (could be done in parallel)
    ne = ue1.shape[1]
    rhs = np.zeros(ue1.shape)
    for iel in range(ne):
        rhs[:, iel] = computerhs1e(iel, ue1, el2nod, flux, a, Minv, S)
    return rhs


def main():
    L = 10.
    ne = 10
    a = 1.
    cfl = 1./10.  # /4.
    Dmax = L  # L/2.0
    tmax = Dmax/a
    alpha = 0.0

    x, el2nod = buildmesh(L, ne)
    #x = np.linspace(0., L, ne+1)
    dx = x[1]-x[0]

    # initial/boundary conditions
    def u0(x): return np.sin(2*np.pi*x/L)

    def inletf(t): return -np.sin(2*np.pi*t/L)
    #u0 = lambda x : 0.0
    #inletf = lambda t : 1.0

    # compute M, Minv and S matrices (same for all elements)
    xx = x[el2nod[:, 0]]  # x for the first element
    M = mass(xx)
    Minv = np.linalg.inv(M)
    S = stiff(xx)
    if 0:
        print 'M =', M
        print 'Minv =', Minv
        print 'S =', S

    xe = np.zeros((2, ne))
    for i in range(ne):
        xe[:, i] = x[el2nod[:, i]]

    # initial condition
    ue1 = np.zeros((2, ne))
    for i in range(ne):
        ue1[:, i] = u0(xe[:, i])

    ue2 = ue1.copy()

    dx = x[1]-x[0]
    dt = cfl*dx/a
    t = 0.
    plotfig(xe, ue2, t)

    print 'dt =', dt, ' tmax =', tmax

    while t+dt < tmax:

        if 0:
            # backward euler
            rhs = computerhs(ue1, inletf, t, a, alpha, el2nod, Minv, S)
            for iel in range(ne):
                ue2[:, iel] = ue1[:, iel] + dt * rhs[:, iel]
        else:
            # rk4
            k1 = computerhs(ue1, inletf, t, a, alpha, el2nod, Minv, S)

            v1 = np.zeros(ue1.shape)
            for iel in range(ne):
                v1[:, iel] = ue1[:, iel] + dt/2 * k1[:, iel]
            k2 = computerhs(v1, inletf, t+dt/2, a, alpha, el2nod, Minv, S)

            v2 = np.zeros(ue1.shape)
            for iel in range(ne):
                v2[:, iel] = ue1[:, iel] + dt/2 * k2[:, iel]
            k3 = computerhs(v2, inletf, t+dt/2, a, alpha, el2nod, Minv, S)

            v3 = np.zeros(ue1.shape)
            for iel in range(ne):
                v3[:, iel] = ue1[:, iel] + dt * k3[:, iel]
            k4 = computerhs(v3, inletf, t+dt, a, alpha, el2nod, Minv, S)

            for iel in range(ne):
                ue2[:, iel] = ue1[:, iel] + dt/6 * \
                    (k1[:, iel]+2*k2[:, iel]+2*k3[:, iel]+k4[:, iel])

        t += dt

        plotfig(xe, ue2, t)
        ue2, ue1 = ue1, ue2


if __name__ == "__main__":
    main()
    # plt.show()
    raw_input('<ENTER TO QUIT>')
