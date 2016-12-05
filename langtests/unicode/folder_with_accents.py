#! /usr/bin/env python
# -*- coding: latin-1 -*-



import os, os.path,sys
foldername = 'àé_dir'

print "sys.stdout.encoding:", sys.stdout.encoding  # cp850 sous windows 10
print "sys.getfilesystemencoding()", sys.getfilesystemencoding()  # mbcs sous windows 10

if not os.path.isdir(foldername):
    os.mkdir(foldername)
else:
    msg=u"%s already exists" % foldername.decode(sys.getfilesystemencoding())
    print "msg=", msg
    print "type(msg)=", type(msg)
    print msg.encode('cp850')
    
for f in os.listdir('.'):
    uf = f.decode(sys.getfilesystemencoding())
    print uf
    if os.path.isdir(uf):
        os.chdir(uf)
        print u"in %s" % uf
    