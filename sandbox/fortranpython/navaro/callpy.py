#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
import numpy as np 
import navaro as na

def fonction(i): 
    print('f(i=%d)=%d' %(i, i*i))
    return i*i

s = na.sommef(fonction, 3)  # appelle "fonction(i)"

print("somme =", s)