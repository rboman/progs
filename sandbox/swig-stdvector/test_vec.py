#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys, os
sys.path.append(os.path.join(os.path.dirname(__file__),'build'))
import vec

v = vec.vector_double()
v.push_back(1.0)
v.push_back(2.0)
print(v[0], v[1])
print(v.size())

