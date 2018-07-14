#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# sorts and prints all files in current folder according to their modification time.


dirs = ('.', )
exts = ('*.*', )

import os
import fnmatch
import time

fmap = []

print "building file list..."
for dir in dirs:
    for path, subdirs, files in os.walk(dir):
        # files.extend(subdirs)
        for name in files:
            for ext in exts:
                if fnmatch.fnmatch(name, ext):
                    fullname = os.path.join(path, name)
                    try:
                        mtime = os.path.getmtime(fullname)
                    except:
                        print "cannot find", fullname, '?'
                    fmap.append((mtime,fullname))
                    #print fullname, mtime
from operator import itemgetter

for f in sorted(fmap,key=itemgetter(0)):
    #print time.asctime(time.gmtime(f[0])), f[0], f[1]    
    print time.strftime('%y-%m-%d (%H:%M:%S)', time.gmtime(f[0])), f[1]