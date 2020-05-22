#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import division
from past.utils import old_div
from couplage import *

p = Problem()

p.getBar().k = 170
p.getBar().rho = 2300
p.getBar().cv = 711
p.getBar().E = 1.58e11
p.getBar().alpha = 2.5e-3
p.getBar().T0 = 273
p.getBar().L = 1.

p.getLight().Q = 1e4
p.getLight().f = old_div(1. / 3600, 24)

p.getMesh().m = 41

tfin = 100000.
p.getNewmark().dt = 10.  # ca commence a osciller a 3. pour 100 mailles
p.getNewmark().nt = int(old_div(tfin, p.getNewmark().dt))
p.getNewmark().gamma = 1.
p.getNewmark().beta = 0.6

p.getResFiles().freq = 10000
p.getPlotWin().freq = 100

rep = os.path.splitext(os.path.basename(__file__))[0]
if not os.path.isdir(rep):
    os.mkdir(rep)
os.chdir(rep)

p.solve()
