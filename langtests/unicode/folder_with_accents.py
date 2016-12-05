#! /usr/bin/env python
# -*- coding: latin-1 -*-



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
    
