#!/usr/bin/env python
# -*- coding: utf-8 -*-
# figure matplotlib "propres"


import numpy as np
import matplotlib.pyplot as plt
import math

# generate some data
scaling = 3.3e-5

x = np.linspace(-3*np.pi, 3*np.pi, 100)
y1 = np.sin(x) * scaling
y2 = (np.sin(2*x)+np.cos(x))/2 * scaling

# figure 1 - default

#fig = plt.figure(figsize=(4,3)) 
fig = plt.figure() 
plt.plot(x*scaling, y1)
plt.plot(x*scaling, y2)
plt.xlabel('xlabel')
plt.ylabel('ylabel')
plt.ticklabel_format(style='sci', scilimits=(0,0))
plt.tight_layout()
fig.show()
fig.savefig('figmatlab1.eps')
fig.savefig('figmatlab1.pdf')

raw_input('<PRESS ENTER>')
