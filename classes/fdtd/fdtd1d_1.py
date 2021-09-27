#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright 2020 University of Liège
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


import math
import numpy as np
# from fwk.wutils import parseargs
# args = parseargs()
# if not args.nogui:
import matplotlib.pyplot as plt

SIZE=200

ez = np.zeros(SIZE, dtype=float)
hy = np.zeros(SIZE, dtype=float)
imp0 = 377.0
maxTime = 1000

result = np.zeros(maxTime, dtype=float)

for qTime in range(maxTime):
    for mm in range(0,SIZE-1):
        hy[mm] = hy[mm] + (ez[mm+1] - ez[mm]) / imp0
    for mm in range(1,SIZE):
        ez[mm] = ez[mm] + (hy[mm] - hy[mm-1]) * imp0
    
    ez[0] = math.exp(-(qTime-30.)*(qTime-30.)/100.)

    #print(ez[50])
    result[qTime] = ez[50]

#
# if not args.nogui:
plt.plot(result)
plt.title('Example p42')
plt.xlabel('time step')
plt.ylabel('Ez[50]')
plt.ylim(-1,1)
plt.grid(True)
plt.show()
