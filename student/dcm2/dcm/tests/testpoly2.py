#!/usr/bin/env python
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



from dcm import Polynome

p = Polynome(5)
p[0]=2
p[1]=-10
p[2]=5
p[3]=-2
p[4]=2

print "p =", p
print "p(3) =", p(3)

d = p.derive()
print "dp/dX =", d

print "sum=", -p+d
print "degree=", len(p+d)

print "p(1)=",p(1)

print "2*p=", 2*p
#print "p*2=", p*2  # pas implémenté en c++
