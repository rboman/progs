#!/usr/bin/env python
# -*- coding: latin-1 -*-



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
