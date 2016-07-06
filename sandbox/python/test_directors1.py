#! /usr/bin/env python
# -*- coding: latin-1; -*-

from wrap import *
from toolbox.conge import Pt
import math

metafor = Metafor()
ale = None

class MyReZoner(CurveReZoner):
    """ Python custom rezoner
    """
    def __init__(self, curve):
        CurveReZoner.__init__(self, curve)

    def execute(self):
        print "MyReZoner.execute()"
        print self.getCurve()
    def __del__(self):
        print "i'm dead!"


l = Line(1,1,1)

def main(): 

    print "beginning of main()"
    global metafor, ale,l
    ale = AleMethod(metafor)
    
    rez = ale.getReZoningStep()
    
    print "creation rezoner"
    myrez = MyReZoner(l)
    print "rez.add"
    rez.add(myrez)
    
    print dir(myrez)
    myrez.execute()
    print "end of main()"
    
    

main()

#ale.getReZoningStep().getReZoner().execute()
    