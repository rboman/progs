#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

import mico 

v = np.zeros(101)
u = np.zeros(101)
P = np.zeros(101)
mico.newtonr(v, u, P)



fig = plt.figure() 
plt.plot(v,P)
plt.grid()
plt.xlabel('vertical displacement (v) [mm]')
plt.ylabel('vertical force (P) [N]')
plt.title("P(v)")
plt.tight_layout()
fig.show()
fig.savefig('pv.png')

fig = plt.figure() 
plt.plot(v,u)
plt.grid()
plt.xlabel('vertical displacement (v) [mm]')
plt.ylabel('horizontal displacement (u) [N]')
plt.title("u(v)")
plt.tight_layout()
fig.show()
fig.savefig('uv.png')


input('<PRESS RETURN>')