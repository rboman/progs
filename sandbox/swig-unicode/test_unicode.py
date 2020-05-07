# -*- coding: utf-8 -*-

from __future__ import print_function
import os, sys

# strings in script
# -----------------
name = 'Romain'
print('type(name) =', type(name)) 
# displays type(name) = <class 'str'> in both python 2 and 3
# but "str" of python 2 means "bytes" of python 3
#     "str" of python 3 means "unicode" of python 2

# strings from python
# -------------------
print(sys.version)
print('type(sys.version) =', type(sys.version)) 



# strings from os
# ---------------

files = os.listdir('.')
print(files)
for f in files:
    print('{} {}'.format(f, type(f)))
afile = os.path.abspath(files[0])
print('{} {}'.format(afile, type(afile)))

# strings from files
# ------------------