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


import sph
from tbox import Vector3d
from fwk import Timer


# test dofs --
d1 = sph.Dofs(Vector3d(0,0,0), Vector3d(0,0,0), 0.0)
d2 = sph.Dofs(Vector3d(1,1,1), Vector3d(0,0,0), 0.0)
print("dof1:", d1)
print("dof2:", d2)

#


boxL   = 2.
Lfloor = 0.7
Lwater = 0.5
sep = 0.05
rho = 1000.0
h0 = 1.2*sep


pbl = sph.Problem(h0)
print(pbl)

# generate a set of particles

import sph.genp as genp

no=0
# mobile particles
cube = genp.Cube( o=(((boxL-Lwater)/2),((boxL-Lwater)/2), ((boxL)/2)+0.5), L=(Lwater,Lwater,Lwater), s=sep)
for p in cube.generate():
    no+=1
    pbl.addMobileP(no, sph.Dofs(Vector3d(p[0], p[1], p[2]), Vector3d(0.0, 0.0, 0.0), rho))


# fixed particles

plane = genp.Cube( o=(((boxL-Lfloor)/2),((boxL-Lfloor)/2), (boxL/2)), L=(Lfloor,Lfloor,sep), s=sep)
for p in plane.generate():
    no+=1
    pbl.addMobileP(no, sph.Dofs(Vector3d(p[0], p[1], p[2]), Vector3d(0.0, 0.0, 0.0), rho))

plane = genp.Cube( o=(0,0,0), L=(boxL,boxL,sep), s=sep)
for p in plane.generate():
    no+=1
    pbl.addMobileP(no, sph.Dofs(Vector3d(p[0], p[1], p[2]), Vector3d(0.0, 0.0, 0.0), rho))

print(pbl)

# kernel

kernel = sph.CubicSplineKernel()
print(kernel)


# sorter

timer = Timer(); timer.start()
sorter = sph.LouisSorter(boxL, h0, kernel)
sorter.execute(pbl.prts)
timer.stop(); print(timer)

print(sorter)

if 0:
    for i in range(sorter.size()):
        for j in range(sorter.size()):
            for k in range(sorter.size()):
                prts = sorter.getParticles(i,j,k)
                if len(prts)!=0:
                    print("(i,l,k)=(%d,%d,%d) nbprts=%d" % (i,j,k,len(prts)))

def saveNeighbours(pbl, fname):
    f = open(fname,'w')
    for p in pbl.prts:
        nos=[]
        for pn in p.neighs:
            nos.append(pn.no)
        f.write( ' p#%d (%d): ' % (p.no, len(p.neighs)))
        for no in sorted(nos):
            f.write( '%d,' % no)
        f.write('\n')
    f.close()
    print('%s saved to disk' % fname)

saveNeighbours(pbl, 'louissorter.txt')

timer = Timer(); timer.start()
sorter = sph.BruteForceSorter(h0, kernel)
sorter.execute(pbl.prts)
timer.stop(); print(timer)


print(sorter)

saveNeighbours(pbl, 'bruteforcesorter.txt')
