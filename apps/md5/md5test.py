#! /usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
from PyQt4 import QtCore

texte = "grape"

blah = QtCore.QString(QtCore.QCryptographicHash.hash((texte),QtCore.QCryptographicHash.Md5).toHex())

print("Qt (1)          =", blah)



hash = QtCore.QCryptographicHash(QtCore.QCryptographicHash.Md5)
hash.addData("gr")
hash.addData("a")
hash.addData("pe")
print("Qt (2)         =", hash.result().toHex())

import md5
m = md5.new()
m.update("gra")
m.update("pe")
print("python md5     =", m.hexdigest())


import hashlib
m = hashlib.md5()
m.update("grap")
m.update("e")
print("python hashlib =", m.hexdigest())

"""
import hashlib
m = hashlib.sha224()
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule?")
print "essai5 =", m.hexdigest()
"""

