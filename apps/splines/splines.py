#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import matplotlib.pyplot as plt
import numpy as np
import math


class Pt:
    """ A simple point class
    """

    def __init__(self, x, y, z):
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)

    def __str__(self):
        return '(' + str(self.x) + ',' + str(self.y) + ',' + str(self.z) + ')'

    def __add__(self, pt):
        return Pt(self.x+pt.x, self.y+pt.y, self.z+pt.z)

    def __sub__(self, pt):
        return Pt(self.x-pt.x, self.y-pt.y, self.z-pt.z)

    def __div__(self, scalar):
        return Pt(self.x//scalar, self.y//scalar, self.z//scalar)

    def __truediv__(self, scalar):
        return Pt(self.x/scalar, self.y/scalar, self.z/scalar)

    def __mul__(self, obj):
        if isinstance(obj, Pt):
            return self.x*obj.x + self.y*obj.y + self.z*obj.z
        else:
            return Pt(self.x*obj, self.y*obj, self.z*obj)

    def __rmul__(self, scalar):
        return self.__mul__(scalar)

    def __abs__(self):
        return math.sqrt(self*self)

    def normalize(self):
        return self/abs(self)

    def min(self, obj):
        if obj.x < self.x:
            self.x = obj.x
        if obj.y < self.y:
            self.y = obj.y
        if obj.z < self.z:
            self.z = obj.z

    def max(self, obj):
        if obj.x > self.x:
            self.x = obj.x
        if obj.y > self.y:
            self.y = obj.y
        if obj.z > self.z:
            self.z = obj.z

    def copy(self):
        return Pt(self.x, self.y, self.z)

    def __gt__(self, obj):
        return (self.x > obj.x and self.y > obj.y and self.z > obj.z)

    def __lt__(self, obj):
        return (self.x < obj.x and self.y < obj.y and self.z < obj.z)

    def __ge__(self, obj):
        return (self.x >= obj.x and self.y >= obj.y and self.z >= obj.z)

    def __le__(self, obj):
        return (self.x <= obj.x and self.y <= obj.y and self.z <= obj.z)


class Seg:
    def __init__(self, x1, x2, u1, u2):
        self.x1 = x1
        self.x2 = x2
        self.u1 = u1
        self.u2 = u2

    def psi1(self, t):  # Hermite functions
        return (1.0+2*t)*(1.0-t)*(1.0-t)

    def psi2(self, t):
        return (3.0-2*t)*t*t

    def psi3(self, t):
        return t*(1.0-t)*(1.0-t)

    def psi4(self, t):
        return (t-1.0)*t*t

    def length(self):
        dx = self.x2-self.x1
        su = self.u2+self.u1
        e = 8.0 - 0.5*(su*su)
        f = dx*su
        g = dx*dx
        T = 3.0*(math.sqrt(f*f+2.*g*e)-f)/e
        return T

    def eval(self, t):
        T = self.length()
        x = self.x1 * self.psi1(t) + self.x2 * self.psi2(t) \
            + self.u1 * T * self.psi3(t) + self.u2 * T * self.psi4(t)
        return x


if __name__ == "__main__":
    p1 = Pt(0, 0, 0)
    p2 = Pt(1, 1, 0)
    u1 = Pt(2, 0, 0)
    u2 = Pt(1, 0, 0)
    s = Seg(p1, p2, u1, u2)

    ts = np.linspace(0.0,1.0,50)
    sx = np.zeros_like(ts)
    sy = np.zeros_like(ts)
    sz = np.zeros_like(ts)
    for i,t in enumerate(ts):
        p = s.eval(t)
        sx[i] = p.x
        sy[i] = p.y
        sz[i] = p.z

    fig = plt.figure()
    plt.plot(sx, sy)
    plt.grid()
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title("spline")
    fig.show()

    input('<PRESS RETURN>') 
