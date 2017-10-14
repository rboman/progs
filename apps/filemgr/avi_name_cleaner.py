#! /usr/bin/env python
# -*- coding: latin-1 -*-

import os, os.path, sys

def clean(rename=False):
    #for file in os.listdir('.'):
    for root, dirs, files in os.walk('.'):
        for file in files:
            fullfile = os.path.join(root,file)
            if os.path.isfile(fullfile):
                base, ext = os.path.splitext(file)
                ext = ext.lower()
                if ext in ['.avi', '.sub', '.idx', '.srt', '.asf', '.mpg', '.wmv']:
                    base = base.replace('.',' ')
                    base = base.replace('_',' ')
                    base = base.replace('cd1','CD1')
                    base = base.replace('cd2','CD2')
                    #base = base.title()
                    newfile = base+ext
                    if file!=newfile:
                        if rename:
                            print "renaming",
                            os.rename(fullfile, os.path.join(root,newfile))
                        print "'%s' => '%s'" % (file,newfile)
            
   


if __name__=="__main__":
    rename=False
    for arg in sys.argv[1:]:
        if arg=="rename":
            rename=True
    clean(rename)
    print "\n[ENTER]"    
    raw_input()    
