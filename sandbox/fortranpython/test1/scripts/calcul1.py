
import numpy as np
import math

import vect
print dir(vect)

vect.norme(math.sin(0.5),math.cos(0.5))

print "----fib----"
print vect.fib.__doc__
a = np.zeros(8,'d')
vect.fib(a)
print a

print "----fib2----"
print vect.fib2.__doc__
print vect.fib2(10)


print "retour a python"
