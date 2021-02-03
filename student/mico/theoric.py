#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

import mico

print(dir(mico))

print('h=', mico.pars.h)

print('ivmax=', mico.pars.ivmax)
print('ivmaxl=', mico.pars.ivmaxl)
ivmax = mico.pars.ivmax
ivmaxl = mico.pars.ivmaxl

u = np.zeros(ivmax)
v = np.zeros(ivmax)
P = np.zeros(ivmax)
ul = np.zeros(ivmaxl)
vl = np.zeros(ivmaxl)
Pl = np.zeros(ivmaxl)
mico.theoric(v, u, P, vl, ul, Pl)


fig = plt.figure()
plt.plot(v, P)
plt.plot(vl, Pl)
plt.grid()
plt.xlabel('vertical displacement (v) [mm]')
plt.ylabel('vertical force (P) [N]')
plt.title("P(v)")
plt.ticklabel_format(style='sci', scilimits=(0, 0))
plt.tight_layout()
fig.show()
fig.savefig('pv.png')

fig = plt.figure()
plt.plot(v, u)
plt.plot(vl, ul)
plt.grid()
plt.xlabel('vertical displacement (v) [mm]')
plt.ylabel('horizontal displacement (u) [N]')
plt.title("u(v)")
#plt.ticklabel_format(style='sci', scilimits=(0,0))
plt.tight_layout()
fig.show()
fig.savefig('uv.png')


input('<PRESS RETURN>')
