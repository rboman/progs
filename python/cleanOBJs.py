#! /usr/bin/env python
# -*- coding: latin-1; -*-

dirs = ('ITK320BIN','VTKBIN','CSwig320BIN')
exts = ('*.obj','*.ilk','*.ncb','*.suo')

import os, fnmatch

for dir in dirs:
    for path, subdirs, files in os.walk(dir):
        #files.extend(subdirs)
        for name in files:
            for ext in exts:
                if fnmatch.fnmatch(name, ext):
                    print os.path.join(path, name)
                    os.remove(os.path.join(path, name))

