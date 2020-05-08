# -*- coding: utf-8 -*-

# "Future statements are NOT import statements. Future statements change how Python interprets the code." 
# The fun is import __future__ is NOT a future statement, it is a import statement.
from __future__ import print_function
# from __future__ import unicode_literals  
#       => with this statement all strings in the text of this script are unicode in py2!
#       => a='Romain'    is read as     a= u'Romain'
# from __future__ import division
#       => "true division" by default (__truediv__)
#       => floor division => //
# from __future__ import absolute_import
#       => implicit relative import in python 2 is forbidden
import os, sys

print( "sys.getdefaultencoding():", sys.getdefaultencoding())  
# python2 : ascii sous windows 10/linux
# python3 : utf-8
print( "sys.stdout.encoding:", sys.stdout.encoding)  
# python2: cp850 sous windows 10 / UTF-8 sous linux
# python3: utf-8
print( "sys.getfilesystemencoding():", sys.getfilesystemencoding())  
# python2: mbcs sous windows 10 / UTF-8 sous linux
# python3: utf-8

# strings in script
# -----------------
print('\n-- strings in script')

name = 'Romain'
print('type({}) = {}'.format(repr(name), type(name))) 
# displays type('Romain') = <type 'str'> in both python 2 and 3
# displays type(u'Romain') = <type 'unicode'> in python 2 with "from __future__ import unicode_literals"
# but "str" of python 2 means "bytes" of python 3
#     "str" of python 3 means "unicode" of python 2

name = u'Romain'
print('type({}) = {}'.format(repr(name), type(name))) 
# displays type(u'Romain') = <type 'unicode'> in python 2
# displays type('Romain') = <type 'str'> in python 3

name = b'Romain'
print('type({}) = {}'.format(repr(name), type(name))) 
# displays type('Romain') = <type 'str'> in python 2
# displays type(b'Romain') = <type 'bytes'> in python 3


# strings from python
# -------------------
print('\n-- strings from python')

print(sys.version)
print('type(sys.version) =', type(sys.version)) 
# python 3 gives unicode (str)
# python 2 gives encoded byte strings (str)


# strings from filesystem
# -----------------------
print('\n-- strings from the filesystem')

files = os.listdir('.')
# print(files)
for f in files:
    if f.startswith('n'):
        # python 2 prints: nÚnuphõr.txt <type 'str'>
        # python 3 prints: nénuphär.txt <class 'str'>
        print('{} {} {}'.format(f, repr(f), type(f)))
        # in py2, f.decode(sys.getfilesystemencoding()) gives the correct unicode
        # string nénuphär.txt
afile = os.path.abspath(files[0])
print('{} {}'.format(afile, type(afile)))

# strings from files
# ------------------
print('\n-- strings from files')
print('.first try:')
try:
    f = open('nénuphär.txt', 'r')
    print('file opened successfully')
except Exception as e:
    print ('\t',e)
    # [Errno 2] No such file or directory: 'n\xc3\xa9nuph\xc3\xa4r.txt'  (python 2)
    # file opened successfully         (python 3)

print('.second try:')
with open(u'nénuphär.txt'.encode(sys.getfilesystemencoding()), 'rb') as f:
    # in python 2 we read str, thus bytes. It works... 
    # in python 3 'r' means 'rt' and there is an implicit call to decode (cp1252 by default on Windows)
    #              => decoding problem is utf-8 chars are found.
    #      .solution 1: open with encoding='utf-8' [not supported in python 2 but works perfectly]
    #      .solution 2: open in binary (=> reads bytes) and decode manually 
    print('file opened successfully')
    for l in f.readlines():
        if sys.version_info.major==2:
            sys.stdout.write(l.decode('utf-8').encode(sys.stdout.encoding,'ignore')) # bytes as argument
        else:
            sys.stdout.write(l.decode('utf-8'))  # unicode as argument

# from __future__ statements are limited to the current file.
try:
    import nofuture # python 2 OK
except:
    print('"import nofuture" failed!') # python 3

