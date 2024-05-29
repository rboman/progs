#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np 
import navaro as na

na.f90module.test_array()
#print len(na.f90module.farray) # na.f90module.farray = NoneType
na.f90module.init(10)
print(len(na.f90module.farray))
na.f90module.test_array()
