
from castswig import *

a1=A()
a2=A()

b1 = B( [a1,a2] ) # <= OK
b2 = B( [1,2] )   # <= should fail

print(a1)
print(b2)


