#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
from builtins import object
import os, sys, platform


pth = os.path.join('build-py%d'%sys.version_info.major,'bin')
if 'Windows' in platform.uname():
    pth = os.path.join(pth,'Release') 
sys.path.append(pth)
# print(sys.path)
del pth

import leak 

import gc 
#gc.set_debug(gc.DEBUG_LEAK)
print (gc.garbage)
print ('gc.isenabled()=', gc.isenabled())


class B(object):
    def __init__(self,name):
        self.name = name
        print('%s.__init__()' % self.name)
    def __del__(self):
        print('%s.__del__()' % self.name)
b=B('b')
c=B('c')

class ObjA2(object):
    def __init__(self,fct):
        self.fct = fct
    def __del__(self):
        print('ObjA2.__del__()')

class ObjA3(leak.Base):
    def __init__(self,f):
        leak.Base.__init__(self)
        self.f = f
    # def __del__(self):
    #     print('ObjA3.__del__()')
    def fct(self):
        print('overloaded fct:', fct)

d=lambda x: x   # leak with py3!

# b="string"          # ok
# b=1.0               # ok
# b=1                 # ok
# b=[]                # ok
#a = leak.ObjA(c)     # leak with py3! even b is not deleted!
#a = leak.ObjA(d)      # leak with py3! no global object of this module are deleted!
#a = leak.ObjA(lambda x: x)     # marche pas mieux
#a = ObjA2(d)

# directors
a = ObjA3(d)
a.call() # faire ce call change tout en python3 et 2?? !!!

#del a            # appel explicite à "del a" => OK
# probleme: en python3, "del a" n'est pas appelé lors de l'arrêt de l'interpréteur
#           en python2, "del a" est appelé

# ------------------------------------------------------------------------------
# dans Metafor, 
#    - definir un objet de type leak.ObjA dans l'espace global de Metafor
#    - definir un objet global qui pointe vers leak.ObjA (variable metafor)
# ...empeche la destruction du module
# 
# remarque: si leak.ObjA est créé puis détruit dans le module, sans être "gardé" par le module
#           soit directement a travers une variable globale soit indirectement 
#           (une de ses variables globales - metafor - fait référence au leak.ObjA),
#           ca marche et le module appelle les destructeurs de ses variables globales en sortie.
#
# resumé: les variables globales d'un module qui a créé un leak.ObjA qui est resté référencé jusqu'à la fin de
#         l'interpréteur ne seront pas détruites par le PyFinalise de python3.
#         toutes les autres variables de ce module (y compris ce leak.ObjA s'il n'est pas global a ce module 
#         sont détruites correctement!)
#
# solutions:
#    ne pas utiliser de "global metafor" 
#             => metafor est détruit correctement
#             => seules les variables globales du module ne sont pas détruites
#             => si leak.ObjA n'est pas global au module, il sera détruit également mais toujours pas les vars globales du module!
# ------------------------------------------------------------------------------


#print (gc.garbage)
#gc.set_debug(gc.DEBUG_LEAK)
# gc.collect()

# import sys
# print('sys.getrefcount(b) =', sys.getrefcount(b))
# lst = gc.get_referrers(b)

# print('XXX:', lst)
