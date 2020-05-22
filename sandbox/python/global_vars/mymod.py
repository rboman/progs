from __future__ import print_function

from builtins import object
class A(object):
    def __init__(self, name):
        self.name=name
        print('\tcreate', self.name)
    def __del__(self):
        print('\tdelete', self.name)

aG = None
a2 = A('a2')

def func():
    global aG
    #if aG==None: aG=A('aG')
    if aG: return aG
    aG = A('aG')
    return aG
