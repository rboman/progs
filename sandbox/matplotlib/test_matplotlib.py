#! /usr/bin/env python
# -*- coding: latin-1 -*-

import numpy as np
import matplotlib.pyplot as plt

x = np.arange(0.0, 2*np.pi, 0.1)
y = np.sin(x)
plt.plot(x,y, 'bo', x,y,'k')
plt.xlabel('x')
plt.ylabel('sin(x)')
plt.title('My plot') 
plt.axis([-1.0, 7.0, -1.1, 1.1])
plt.grid(True)
plt.show()

