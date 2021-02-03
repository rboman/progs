#! /usr/bin/env python3
# -*- coding: utf-8 -*-

""" Test several ways of creating MD5 hash (with Qt / python)
"""

from PyQt5 import QtCore

texte = b"grape"

blah = QtCore.QCryptographicHash.hash((texte),QtCore.QCryptographicHash.Md5).toHex()
print("Qt (1)         =", blah)


hash = QtCore.QCryptographicHash(QtCore.QCryptographicHash.Md5)
hash.addData(b"gr")
hash.addData(b"a")
hash.addData(b"pe")
print("Qt (2)         =", hash.result().toHex())


# import md5          # not available in python 3
# m = md5.new()
# m.update(b"gra")
# m.update(b"pe")
# print("python md5     =", m.hexdigest())


import hashlib
m = hashlib.md5()
m.update(b"grap")
m.update(b"e")
print("python hashlib =", m.hexdigest())


