#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import numpy as np 
import navaro as na

na.f90module.test_array()
#print len(na.f90module.farray) # na.f90module.farray = NoneType
na.f90module.init(10)
print(len(na.f90module.farray))
na.f90module.test_array()
