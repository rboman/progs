#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# see https://matplotlib.org/tutorials/index.html

import matplotlib.pyplot as plt
import numpy as np

t = np.arange(0.0, 2.0, 0.01)
s = np.sin(2*np.pi*t)
c = np.cos(2*np.pi*t)
plt.plot(t, s, label='sin')
plt.plot(t, c, label='cos')

plt.xlabel('time (s)')
plt.ylabel('voltage (mV)')
plt.title('About as simple as it gets, folks')
plt.grid(True)
plt.legend()
#plt.savefig("test.png")
plt.show()
