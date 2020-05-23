#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from PyQt4 import QtCore

texte = "hello ca roule hello ca roule hello ca roule hello ca roule?"

blah = QtCore.QString(QtCore.QCryptographicHash.hash((texte),QtCore.QCryptographicHash.Md5).toHex())

print("essai1 =", blah)



hash = QtCore.QCryptographicHash(QtCore.QCryptographicHash.Md5)
hash.addData("hello ca roule ")
hash.addData("hello ca roule ")
hash.addData("hello ca roule ")
hash.addData("hello ca roule?")
print("essai2 =", hash.result().toHex())

import md5
m = md5.new()
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule?")
print("essai3 =", m.hexdigest())


import hashlib
m = hashlib.md5()
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule?")
print("essai4 =", m.hexdigest())


import hashlib
m = hashlib.sha224()
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule ")
m.update("hello ca roule?")
print("essai5 =", m.hexdigest())


