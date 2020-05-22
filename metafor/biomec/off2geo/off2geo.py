#! /usr/bin/env python3
#
# conversion du format OOGL .off (sortie d'Isosurf) en .geo (entree Gmsh)
# 
#

from __future__ import print_function
from builtins import range
import sys
#print 'args=', sys.argv

basename ="brain"
lc=4

inFile = open( basename+'.off', 'r')
tag = inFile.readline()
if tag[0:4]!="NOFF":
    print("bad format!")
    sys.exit()
line = inFile.readline().split()
nbnod = int(line[0])
nbelm = int(line[1])
#print 'reading',nbnod,'nodes and',nbelm,'elements'

print("lc=",lc,";")

for i in range(0,nbnod):
    line=inFile.readline().split()
    print("Point(%d)={%e,%e,%e,lc};" % (i+1, float(line[0]), float(line[1]), float(line[2])))

edges={}
nextno=1
for i in range(0,nbelm):
    line=inFile.readline().split()
    if int(line[0])!=3:
        print("bad elem (%d nodes)" % int(line[0]))
        sys.exit()
    n1=int(line[1])+1
    n2=int(line[2])+1
    n3=int(line[3])+1
    eds = [(n1,n2),(n2,n3),(n3,n1)]
    edno = [0, 0, 0]
    for j in range(0,3):
        edge=eds[j];
        sign=1
        edno[j]=0
        if edge[0]>edge[1]:
            sign=-1
            edge=(edge[1],edge[0])
        if edge in edges:
            edno[j]=edges[edge]*sign
        else:
            edno[j]=nextno*sign
            edges[edge]=nextno
            print("Line(%d) = {%d,%d};" %(nextno, edge[0], edge[1]))
            nextno=nextno+1
    print("Line Loop(%d) = {%d,%d,%d};" % (nextno, edno[0], edno[1], edno[2]))
    print("Plane Surface(%d) = {%d};" % (i+1, nextno))
    nextno=nextno+1

print("Coherence;")
print("Surface Loop(%d) = {" % (nbelm+1), end=' ')
for i in range(0,nbelm):
    print(i+1, end=' ')
    if i==nbelm-1:
        print("};")
    else:
        print(",", end=' ')
print("Volume(1) = {%d};" % (nbelm+1))
print("Physical Volume(1) = {1};")

print("Mesh.CharacteristicLengthFactor = ", lc, ";")



