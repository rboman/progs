#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import numpy as np 
import navaro as na

geom = na.mesh.create(0.0, 10, 0.1)
print('geom=', geom)
#na.mesh.view()
na.mesh.view(geom)
