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


import numpy as np
# from fwk.wutils import parseargs
# args = parseargs()
# if not args.nogui:
import matplotlib.pyplot as plt
import matplotlib.animation as animation

SIZE = 200

ez = np.zeros(SIZE, dtype=float)
hy = np.zeros(SIZE, dtype=float)
imp0 = 377.0
maxTime = 300
nloop = 1

# if not args.nogui:
fig, ax = plt.subplots()
line, = plt.plot(ez)
plt.title('Example p42')
plt.xlabel('X')
plt.ylabel('Ez')
#plt.ylim(-2,2)
plt.ylim(-1,1)
plt.grid(True)
plt.draw()
# else:
#     line = None

qTime=0
def animate(i): 
    global qTime
    for l in range(nloop):
        qTime=qTime+1

        # ----- Hy -------
        # absorbing boundary condition (ABC)
        hy[SIZE-1] = hy[SIZE-2]
        # perfect magnetic conductor (PMC)
        # hy[SIZE-1] = 0
        for mm in range(0,SIZE-1):
            hy[mm] = hy[mm] + (ez[mm+1] - ez[mm]) / imp0

        # correction for TFSF boundary
        hy[49] -= np.exp(-(qTime - 30.)*(qTime - 30.)/100.) / imp0


        # ----- Ez -------
        # absorbing boundary condition (ABC)
        ez[0] = ez[1] 
        # prescribed source (or perfect electric conductor if =0 (PEC) )
        #ez[0] = np.exp(-(qTime-30.)*(qTime-30.)/100.)
        for mm in range(1,SIZE):
            ez[mm] = ez[mm] + (hy[mm] - hy[mm-1]) * imp0

        # additive source
        #ez[50] += np.exp(-(qTime-30.)*(qTime-30.)/100.)
        # correction for TFSF boundary
        ez[50] += np.exp(-(qTime+0.5- (-0.5) - 30.)*(qTime+0.5- (-0.5) - 30.)/100.)

    # if not args.nogui:
    line.set_ydata(ez)
    ax.set_title('time = %d' % qTime)

    return line,

# if not args.nogui:
ani = animation.FuncAnimation(fig, animate, maxTime // nloop, 
                                interval=25, repeat=False)
plt.show()
# else:
#     # calls the loops without matplotlib
#     for i in range(maxTime // nloop):
#         animate(i)