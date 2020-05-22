#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
import numpy as np 

import navaro as na
print(dir(na))
print(na.norme.__doc__)

a = np.arange(1, 5)
print("a=", a)
print("norme=", na.norme(a), np.linalg.norm(a))

