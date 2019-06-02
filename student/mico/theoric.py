#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

import mico 

print dir(mico)

print 'h=', mico.pars.h

u = np.zeros(101)
v = np.zeros(101)
P = np.zeros(101)
ul = np.zeros(16)
Pl = np.zeros(16)
mico.theoric(v, u, P, ul, Pl)


fig = plt.figure() 
plt.plot(v[0:90], P[0:90])
plt.plot(np.linspace(0,15,16), Pl)
plt.grid()
plt.xlabel('vertical displacement (v) [mm]')
plt.ylabel('vertical force (P) [N]')
plt.title("P(v)")
plt.ticklabel_format(style='sci', scilimits=(0,0))
plt.tight_layout()
fig.show()
fig.savefig('pv.png')

fig = plt.figure() 
plt.plot(v[0:90],u[0:90])
plt.plot(np.linspace(0,15,16), ul)
plt.grid()
plt.xlabel('vertical displacement (v) [mm]')
plt.ylabel('horizontal displacement (u) [N]')
plt.title("u(v)")
#plt.ticklabel_format(style='sci', scilimits=(0,0))
plt.tight_layout()
fig.show()
fig.savefig('uv.png')


raw_input('<PRESS RETURN>')

