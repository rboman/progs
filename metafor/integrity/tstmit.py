# fichier de test du mailleur transfini 3D
# 12 lignes

from wrap import *


domain = Domain()
domain.setAnalysis(METAFOR_ID)

geo = domain.getGeometry()
geo.setDim3D()

pset = geo.getPointSet()

pset.define(1, 0.0,0.0,0.0)
pset.define(2, 1.0,0.0,0.0)
pset.define(3, 1.0,1.0,0.0)
pset.define(4, 0.0,1.0,0.0)
pset.define(5, 0.0,0.0,1.0)
pset.define(6, 1.0,0.0,1.0)
pset.define(7, 1.0,1.0,1.0)
pset.define(8, 0.0,1.0,1.0)

pset.define(101, 0.3, 0.0, 0.0)
pset.define(102, 0.6, 0.0, 0.0)

cset = geo.getCurveSet()


cset.copy( Line(101, 1, 101) )
cset.copy( Line(102, 101, 102) )
cset.copy( Line(103, 102, 2) )

cset.copy( Line(2, 3, 2) )
cset.copy( Line(3, 3, 4) )
cset.copy( Line(4, 4, 1) )

cset.copy( Line(5, 6, 5) )
cset.copy( Line(6, 6, 7) )
cset.copy( Line(7, 7, 8) )
cset.copy( Line(8, 8, 5) )

cset.copy( Line(10, 5, 1) )
cset.copy( Line(11, 6, 2) )
cset.copy( Line(12, 3, 7) )
cset.copy( Line(13, 8, 4) )

wset = geo.getWireSet()
#w = Wire(1); w.push(2,3,4,1); wset.copy(w) 
w = Wire(1); w.push(102,103,2,3,4,101); wset.copy(w) 
w = Wire(2); w.push(5,6,7,8); wset.copy(w) 
w = Wire(3); w.push(103,11,5,10,101,102); wset.copy(w) 
#w = Wire(3); w.push(1,11,5,10); wset.copy(w) 
w = Wire(4); w.push(2,12,6,11); wset.copy(w) 
w = Wire(5); w.push(13,3,12,7); wset.copy(w) 
w = Wire(6); w.push(4,10,8,13); wset.copy(w) 


sdset = geo.getSideSet()
for i in range(1,7):
    s = Side(i); s.push(i); sdset.copy(s) 






skset = geo.getSkinSet()
#s = Skin(1); s.push(1,2,3,4,5,6); skset.copy(s) 
s = Skin(1); s.push(6,1,2,5,4,3); skset.copy(s) 

vset = geo.getVolumeSet()
s=Volume(1); s.push(1); vset.copy(s) 

# mesh

cset(101).mesh(3)
cset(102).mesh(10)
cset(103).mesh(4)

for i in [3, 5, 7]:
    cset(i).mesh(17)
for i in [2, 4, 6, 8]:
    cset(i).mesh(5)
for i in [10, 11, 12, 13]:
    cset(i).mesh(4)

sdset(1).mesh( ((102,101,103), 3,4,2), False )
sdset(3).mesh( ((103,102,101), 5,11,10), False )
for i in [2,4,5,6]:
    sdset(i).mit(False)

#sdset(5).mit(True)

#vset(1).mit(True)

#vset(1).mesh( ((103,101,102),2,3,4,5,6,7,8,10,11,12,13) ) # ok
#vset(1).mesh( (5,11,(103,101,102),10,7,12,3,13,8,6,2,4) ) # ok aussi!
vset(1).mesh( (6,12,2,11,8,13,4,10,5,7,3,(101,102,103)) ) # ok aussi!

win=VizWin()
win.add(pset)
win.add(cset)
#win.add(domain.getTopology().getPointSet())
#win.add(domain.getTopology().getSideSet())
win.add(domain.getTopology().getVolumeSet())
win.open()
input()



