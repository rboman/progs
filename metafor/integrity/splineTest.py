
from wrap import *


metafor = Metafor()
domain = metafor.getDomain()
geometry = domain.getGeometry()
geometry.setDim3D()


poiset   = geometry.getPointSet()
poiset.define(1,  -100, 0, 0)
poiset.define(2,  10, 2, 0)
poiset.define(3,  20, -2, 0)
poiset.define(4,  22, 2, 0)
poiset.define(5,  24, 0, 0)
poiset.define(6,  26, 3, 0)
poiset.define(7,  30, 0, 0)
poiset.define(8,  170, 10, 0)

eps = 10

poiset.define(11,  -100, 0, +eps)
poiset.define(12,  10, 2, +eps)
poiset.define(13,  20, -2, 0+eps)
poiset.define(14,  22, 2, +eps)
poiset.define(15,  24, 0, +eps)
poiset.define(16,  26, 3, +eps)
poiset.define(17,  30, 0, 0+eps)
poiset.define(18,  170, 10, +eps)

curset   = geometry.getCurveSet();

spl1 = CubicSpline(1)
for i in (1,2,3,4,5,6,7,8):
    spl1.push(poiset(i))
curset.add(spl1)

spl2 = CubicSpline(11)
for i in (11,12,13,14,15,16,17,18):
    spl2.push(poiset(i))
spl2.useLittTangents()
curset.add(spl2)

win=VizWin()
win.add(poiset)
win.add(curset)
win.open()

print("press ENTER...")
input()
