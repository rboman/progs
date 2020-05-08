# -*- coding: utf-8 -*-

# ----------------------------------------------------------------
# setup the full "future" environment
# ----------------------------------------------------------------
from __future__ import (absolute_import, division,
                        print_function, unicode_literals)
from builtins import *
from future.builtins.disabled import *
from future.standard_library import install_aliases
install_aliases()

from future.utils import native_str
from future.utils import native
# ----------------------------------------------------------------

import os, sys

if sys.version_info.major==2:
    sys.path.append(os.path.join('build-py2','bin','Release'))
else:
    sys.path.append(os.path.join('build-py3','bin','Release'))

import _unic 

print('{} {}'.format("type('chaine')",type('chaine')))  # unicode on py2 (because of unicode_literals)
print('{} {}'.format("type(native('chaine'))",type(native('chaine'))))  # unicode on py2
print('{} {}'.format("type(native_str('chaine'))",type(native_str('chaine')))) # str on py2

# fct expects "str" in both case, which means unicode in py3 and bytes in py2!
_unic.fct(native_str('chaine')) # works on both py2 and py3!!

str2 = str('chaine2')

print('{} {}'.format("type(str2)",type(str2)))  # future.types.newstr.newstr on py2 (because of "import str")
print('{} {}'.format("type(native(str2))",type(native(str2))))  # unicode on py2
print('{} {}'.format("type(native_str(str2))",type(native_str(str2)))) # str on py2

# fct expects "str" in both case, which means unicode in py3 and bytes in py2!
_unic.fct(native_str(str2)) # works on both py2 and py3!!