#! /usr/bin/env python
# -*- coding: latin-1 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

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


l = Line(1, 1, 1)


def main():

    print "beginning of main()"
    global metafor, ale, l
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

# ale.getReZoningStep().getReZoner().execute()
