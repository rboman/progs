#! /usr/bin/env python
# -*- coding: latin-1 -*-

# see https://matplotlib.org/tutorials/index.html

import matplotlib.pyplot as plt
import numpy as np

sigy = 169.
A=127.42425
n_1=0.225


e = np.arange(0.0, 0.05, 0.001)
p1= sigy*(1.+A*e)**n_1
p2 = sigy+(A*sigy*n_1)*e
plt.plot(e, p1, label='law')
plt.plot(e, p2, label='linearized')

plt.xlabel('equ. plastic strain')
plt.ylabel('yield stress')
plt.title('Project #36')
plt.grid(True)
plt.ylim(0,400.)
plt.legend()
#plt.savefig("test.png")
plt.show()
