# $Id$
# "print" test

from wrap import *

def printTest(objet):
    for i in range(1,30): print "-", 
    print "\nPrint test of", objet
    for i in range(1,30): print "-",
    print ''
    exec('print '+objet)

#import apps.qs.cont2;
#domain = apps.qs.cont2.getDomain()
import apps.ale.qsUzi;
domain = apps.ale.qsUzi.getDomain(0)
domain.build()

print '\n------------------------------------------\n'

a=Matr2(1,2,3,4)
print a
a=Matr3(1,2,3,4)
print a

win=VizWin();
print win;


def fct(x,y,z):
    print 'plouf'
    print "x=", x
    print "y=", y
    print "z=", z
    
    return x*x+y*y+z*z
pfct = PythonMultiParameterFunction(fct,3)
print pfct

keys = KeyList(Key(TX),Key(TY),Key(TZ),Key(TM))
print keys

print '\n------------------------------------------\n'

#printTest('domain.findObject(ELEMENTSET_ID)')

#printTest('domain.getGeometry().getPointSet()')
#printTest('domain.findObject(TOPOLOGY_ID).getPointSet()')
#printTest('domain.findObject(NODESET_ID)')
printTest('domain.getGeometry().getCurveSet()')
printTest('domain.getGeometry().getCurveSet()(1)')
printTest('domain.getInteractionSet()')
printTest('domain.getInteractionSet()(99)')
printTest('domain.getLoadingSet()')
printTest('domain.getMaterialSet()')
printTest('domain.getMaterialSet()[1]')
printTest('domain.getFixationSet()')
printTest('domain.getDofSet()')
printTest('domain')
printTest('domain.getMetafor().get(0)')
#printTest('domain.getPartition()')
#printTest('domain.getConnexion12()')
#printTest('domain.getMetafor().get(0).findDBSet(TX|RE)')
#printTest('domain.findDBSet(TX|AB)')
printTest('domain.getMetafor()')
printTest('domain.getMetafor().getAleMethod()')
printTest('domain.getMetafor().getAleMethod().getReZoningStep()')

