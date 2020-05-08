# -*- coding: utf-8 -*-

# ----------------------------------------------------------------
# setup the full "future" environment
# ----------------------------------------------------------------
from __future__ import print_function
# from __future__ import (absolute_import, division,
#                         print_function, unicode_literals)
# from builtins import *
# from future.builtins.disabled import *
# from future.standard_library import install_aliases
# install_aliases()

# from future.utils import native_str
# from future.utils import native
# ----------------------------------------------------------------

# test dict with swig objects as keys in python 2 and python 3
# (requires fcts __hash__, __cmp__ and __eq__ to be defined in the object).

import os, sys

if sys.version_info.major==2:
    sys.path.append(os.path.join('build-py2','bin','Release'))
else:
    sys.path.append(os.path.join('build-py3','bin','Release'))

import unic 

a = unic.ObjA()

alist = unic.ListA()
alist.push(a)

m = {}

m[a] = 1.0

print (m)

refa = alist.get(0)
print (dir(refa))
print ('hash=', refa.__hash__(),a.__hash__() )
print ('__weakref__=', refa.__weakref__,a.__weakref__ )
print ('this=', refa.this,a.this )
print('equal=',  a == refa )
print ('refa=',refa)
print (m[refa])  # calls __hash__ and __cmp__ in python 2
                 # calls __hash__ and __eq__ in python 3


