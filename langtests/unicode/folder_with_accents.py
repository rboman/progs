#! /usr/bin/env python
# -*- coding: latin-1 -*-

#   PyQt           python 2            python 3
#  QString           unicode             str
#  QByteArray        str                 bytes

# http://sametmax.com/lencoding-en-python-une-bonne-fois-pour-toute/

# ISO-8859-1 (also called Latin-1) is identical to Windows-1252 (also called CP1252) 
# except for the code points 128-159 (0x80-0x9F). 
# ISO-8859-1 assigns several control codes in this range. 
# Windows-1252 has several characters, punctuation, arithmetic and business symbols assigned to these code points.

import os, os.path,sys
foldername = u'àé_dir'   # plante sous linux sans le 'u'

print "sys.stdout.encoding:", sys.stdout.encoding  # cp850 sous windows 10 / UTF-8 sous linux
print "sys.getfilesystemencoding():", sys.getfilesystemencoding()  # mbcs sous windows 10 / UTF-8 sous linux

if not os.path.isdir(foldername):
    os.mkdir(foldername)
else:
    msg=u"%s already exists" % foldername
    print "msg=", msg
    print "type(msg)=", type(msg)
    print msg.encode(sys.stdout.encoding)
    
for f in os.listdir('.'):
    uf = f.decode(sys.getfilesystemencoding())
    print uf
    if os.path.isdir(uf):
        os.chdir(uf)
        print u"in %s" % uf
    
