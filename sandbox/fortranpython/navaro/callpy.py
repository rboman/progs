#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np 
import navaro as na

def fonction(i): 
    print 'f(i=%d)=%d' %(i, i*i)
    return i*i

s = na.sommef(fonction, 3)  # appelle "fonction(i)"

print "somme =", s