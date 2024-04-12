#!/usr/bin/env python3

# solve a polynomial eq with numpy/scipy

import numpy as np
from scipy.optimize import fsolve

def f(L):
    return ((L-1)*4+1)*((L-1)*2+1)*L - 123750

x = fsolve(f, 10)

print(f'{x[0] = }')
print(f'{f(x[0]) = }')

