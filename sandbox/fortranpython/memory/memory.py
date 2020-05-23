#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np 
import sandbox as m

print(dir(m))

m.mem.test_array()
m.mem.init(10)
print(len(m.mem.farray))
m.mem.test_array()
