#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import math
from point import Pt


class Seg:
    def __init__(self, x1, x2, u1, u2):
        self.x1 = x1
        self.x2 = x2
        self.u1 = u1
        self.u2 = u2
        self.L = self.length()

    def psi1(self, t):  # Hermite functions
        return (1.0+2*t)*(1.0-t)*(1.0-t)

    def psi2(self, t):
        return (3.0-2*t)*t*t

    def psi3(self, t):
        return t*(1.0-t)*(1.0-t)

    def psi4(self, t):
        return (t-1.0)*t*t

    def length(self):
        """length approximation 
        """
        dx = self.x2-self.x1
        su = self.u2+self.u1
        e = 8.0 - 0.5*(su*su)
        f = dx*su
        g = dx*dx
        T = 3.0*(math.sqrt(f*f+2.*g*e)-f)/e
        return T

    def eval(self, t):
        x = self.x1 * self.psi1(t) + self.x2 * self.psi2(t) \
            + self.u1 * self.L * self.psi3(t) + self.u2 * self.L * self.psi4(t)
        return x


class Spline:
    def __init__(self, pts):
        self.pts = pts
        self.rebuild()

    def rebuild(self):
        # compute tangents from pts
        us = Spline.computeTG(self.pts)
        # fill segment list
        self.segs = []
        for i in range(len(self.pts)-1):
            p1 = self.pts[i]
            p2 = self.pts[i+1]
            u1 = us[i]
            u2 = us[i+1]
            self.segs.append(Seg(p1, p2, u1, u2))

    def lengthTo(self, n):
        """compute the length to segment #n starting from the first point
        """
        L = 0.0
        if n > len(self.segs):
            n = len(self.segs)
        for i in range(n):
            L += self.segs[i].L
        return L

    def length(self):
        """total length of the spline
        """
        return self.lengthTo(len(self.segs))

    def getKsiOnSeg(self, t):
        """ given t in [0,1] on the cubic spline,
        compute the segment number and the local coordinate on this segment
        """
        L = self.length()
        ksiL = t*L

        # look for segment
        T = 0.0
        for n in range(len(self.segs)):
            Tl = self.segs[n].L  # length of seg #n
            Told = T             # starting ksi of the segment
            T += Tl              # ending ksi of the segment
            if (ksiL <= T):
                break

        if(n == len(self.segs)):  # if ksi is beyond the last segment
            n -= 1

        # eval seg #n
        # linear approx (McConalogue => quasi-instrinsic parametrization)
        ksiOnSeg = (ksiL-Told)/Tl
        return n, ksiOnSeg

    def eval(self, t):
        n, ksiOnSeg = self.getKsiOnSeg(t)
        x = self.segs[n].eval(ksiOnSeg)
        return x

    @staticmethod
    def computeTG(pts):
        """compute tangents as a function of pts, a list of points
        """
        us = []

        # starting vertex
        x0 = pts[0]
        x1 = pts[1]
        x2 = pts[2]

        d1 = x1-x0
        d2 = x2-x1

        l1 = abs(d1)
        l2 = abs(d2)

        u = (-l2*(2.*l1+l2)) * x0 \
            + ((l1+l2)*(l1+l2)) * x1 \
            - (l1*l1) * x2
        u = u.normalized()
        us.append(u)

        # slope at next vertices
        for n in range(1, len(pts)-1):
            if(n > 1):
                x0 = x1
                x1 = x2
                x2 = pts[n+1]
            d1 = x1-x0
            d2 = x2-x1
            l1 = d1*d1
            l2 = d2*d2
            alpha = l2/(l1+l2)

            u = (1.0-alpha) * d1 + alpha * d2
            u = u.normalized()
            us.append(u)

        # slope at last vertex

        d1 = x2-x1
        d2 = x1-x0
        l1 = abs(d1)
        l2 = abs(d2)
        u = (l2*(2.*l1+l2)) * x2 \
            - ((l1+l2)*(l1+l2)) * x1 \
            + (l1*l1) * x0
        u = u.normalized()
        us.append(u)

        return us
