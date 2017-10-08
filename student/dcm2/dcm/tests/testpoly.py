#!/usr/bin/env python
# -*- coding: latin-1 -*-

def main():
    from dcm import Polynome
    Polynome.demo()

    print "\n\nidem en python...\n"

    a = Polynome(3)
    a[0] = 4.
    a[1] = 2.
    a[3] = 1.

    b = Polynome(4)    
    b[1] = 1.3
    b[4] = 1.4

    c = a + b
    d = a * b

    print "a(X)    = ", a
    print  "a(.5)   = ", a(.5)
    print "b(X)    = ", b
    print "b(.5)   = ", b(.5)
    print "c(X)=a+b= ", c
    print "c(.5)   = ", c(.5)
    print "d(X)=a*b= ", d
    print "d(.5)   = ", d(.5)
    print "d'(X)   = ", d.derive()
    print (d.primitive())(1) - (d.primitive())(0)
    print d.integrale(0, 1)

if __name__ == "__main__":
    main()
