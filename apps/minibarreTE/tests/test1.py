#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from past.utils import old_div
from couplage import *

p = Problem()

p.getBar().k = 170
p.getBar().rho = 2300
p.getBar().cv = 711
p.getBar().E = 1.58e11
p.getBar().alpha = 2.5e-3
p.getBar().T0 = 273
p.getBar().L = 45e-6

p.getLight().Q = 1e8
p.getLight().f = 1e5

p.getMesh().m = 101

tfin = 6e-4
p.getNewmark().dt = 1e-6
p.getNewmark().nt = int(old_div(tfin, p.getNewmark().dt))
p.getNewmark().gamma = 0.501
p.getNewmark().beta = 0.255


rep = os.path.splitext(os.path.basename(__file__))[0]
if not os.path.isdir(rep):
    os.mkdir(rep)
os.chdir(rep)

p.solve()
