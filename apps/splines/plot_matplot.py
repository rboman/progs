#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import matplotlib.pyplot as plt
import numpy as np
from splines import *


def plot_one_seg():
    """ Test routine: create one segment and plot it with matplotlib
    """
    p1 = Pt(0, 0, 0)
    p2 = Pt(1, 1, 0)
    u1 = Pt(2, 0, 0)
    u2 = Pt(1, 0, 0)
    s = Seg(p1, p2, u1, u2)

    ts = np.linspace(0.0, 1.0, 50)
    sx = np.zeros_like(ts)
    sy = np.zeros_like(ts)
    sz = np.zeros_like(ts)
    for i, t in enumerate(ts):
        p = s.eval(t)
        sx[i] = p.x
        sy[i] = p.y
        sz[i] = p.z

    fig = plt.figure()
    plt.plot(sx, sy)
    plt.grid()
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title("one spline segment")
    plt.axis('equal')
    fig.show()


def plot_one_spline():
    """ Test routine: create one spline and plot it with matplotlib
    """
    p1 = Pt(0, 0, 0)
    p2 = Pt(1, 1.5, 0)
    p3 = Pt(3, 0.5, 0)
    p4 = Pt(2.5, -1.5, 0)
    p5 = Pt(4, 0.0, 0)
    p6 = Pt(4.5, 0.0, 0)
    p7 = Pt(5.0, 0.0, 0)

    s = Spline([p1, p2, p3, p4, p5, p6, p7])

    # curve
    ts = np.linspace(0.0, 1.0, 20*len(s.pts))
    sx = np.zeros_like(ts)
    sy = np.zeros_like(ts)
    for i, t in enumerate(ts):
        p = s.eval(t)
        sx[i] = p.x
        sy[i] = p.y

    # knots
    px = np.zeros_like(s.pts, dtype=float)
    py = np.zeros_like(s.pts, dtype=float)
    for i, p in enumerate(s.pts):
        px[i] = p.x
        py[i] = p.y

    fig = plt.figure()
    plt.plot(sx, sy)
    plt.plot(px, py, 'o')
    plt.grid()
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title("one spline")
    plt.axis('equal')
    fig.show()


if __name__ == "__main__":
    plot_one_seg()
    plot_one_spline()

    input('<PRESS RETURN>')
