#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
from builtins import str
from builtins import object
import sys, glob, os, time
import json
import pprint

"""
def findfiles(patterns):
    for file_or_dir in glob.glob(patterns):
        if os.path.isdir(file_or_dir):
            for path, subdirs, files in os.walk(dir):
                print 'entering "%s"...' % path
                for file in files:
                    convert(file, path)
"""


class File(object):
    def __init__(self, dir, name):
        self.fullname = os.path.join(dir,name)
        self.dir = dir
        self.name = name
        bid, self.ext = os.path.splitext(name)
        self.size = os.path.getsize(self.fullname)
        self.mtime = os.path.getmtime(self.fullname)
    def __str__(self):
        ret = "dir='%s' " % self.dir
        ret += "name='%s' " % self.name
        ret += "ext='%s' " % self.ext
        ret += "size=%d " % self.size
        ret += "mtime='%s' " % time.asctime(time.gmtime(self.mtime))
        
        return ret

class DB(object):

    def __init__(self):
        self.db = []

    def fill(self, basedir):
        print("building db...")
        basedir = os.path.abspath(basedir)
        if os.path.isdir(basedir):
            for path, subdirs, files in os.walk(basedir):
                #print 'entering "%s"...' % path
                for file in files:
                    #print "adding", os.path.join(path, file)
                    self.db.append(File(path, file)  )
        print("...done.")

    def find(self, ext):
        files = []
        for o in self.db:
            if o.ext == ext:
                files.append(o)
        return files

    def __str__(self):
        ret=''
        for o in self.db:
            ret += str(o) + '\n'  
        return ret  

class MyEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, File):
            return { "dir": obj.dir, "name": obj.name}
        # Let the base class default method raise the TypeError
        return json.JSONEncoder.default(self, obj)

def MyHook(d): 
    print("MyHook:", d)
    return File(d['dir'], d['name'])




if __name__ == "__main__":
    #buildDB(".")

    db=DB() 
    db.fill('.')

    js = json.dumps(db.db, indent=4, cls=MyEncoder)
    #print js

    db2 = json.loads(js, object_hook=MyHook)
    #print db2

    #print db
    #pprint.pprint( db.db )
"""
    files = db.find('.m')
    for f in files:
        print f.fullname
    print "%d files" % len(files)
    #printDB()
"""