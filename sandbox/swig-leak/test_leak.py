#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import os, sys, platform


pth = os.path.join('build-py%d'%sys.version_info.major,'bin')
if 'Windows' in platform.uname():
    pth = os.path.join(pth,'Release') 
sys.path.append(pth)
print(sys.path)
del pth

import leak 

import gc 
#gc.set_debug(gc.DEBUG_LEAK)
print (gc.garbage)
print ('gc.isenabled()=', gc.isenabled())


class B:
    def __init__(self,name):
        self.name = name
        print('%s.__init__()' % self.name)
    def __del__(self):
        print('%s.__del__()' % self.name)
b=B('b')
c=B('c')

# class ObjA2:
#     def __init__(self,fct):
#         self.fct = fct
#     def __del__(self):
#         print('ObjA2.__del__()')


d=lambda x: x   # leak with py3!

# b="string"          # ok
# b=1.0               # ok
# b=1                 # ok
# b=[]                # ok
#a = leak.ObjA(c)      # leak with py3! even b is not deleted!
a = leak.ObjA(d)     # leak with py3! b,c not deleted
#a = ObjA2(c)
# del a
#print (gc.garbage)
#gc.set_debug(gc.DEBUG_LEAK)
#gc.collect()

# import sys
# print('sys.getrefcount(b) =', sys.getrefcount(b))
# lst = gc.get_referrers(b)

# print('XXX:', lst)
