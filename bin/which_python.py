#!/usr/bin/env python
# -*- coding: utf-8 -*-

# USAGE: double cliquer sur ce fichier pour voir quel python est utilisé

# C:\Users\r_bom\Downloads>py --list
# Installed Pythons found by py Launcher for Windows
# -3.7-64 *
# -2.7-64

# Associer "py.exe" aux fichiers python:
# ======================================

# Tester d'abord ce qui est lancé pour les .py:

# C:\Users\r_bom\Downloads>assoc .py
# .py=Python.File
# C:\Users\r_bom\Downloads>ftype Python.File
# Python.File="f:\local\python\bin\python.exe" "%1" %*
# C:\Users\r_bom\Downloads>assoc .pyw
# .pyw=Python.NoConFile
# C:\Users\r_bom\Downloads>ftype Python.NoConFile
# Python.NoConFile="f:\local\python\bin\pythonw.exe" "%1" %*

# Ensuite, modifier dans un cmd.exe admin:

# assoc .py=Python.File              (au cas où ce serait pas bon)
# ftype Python.File="c:\Windows\py.exe" "%1" %*
# ftype Python.NoConFile="c:\Windows\pyw.exe" "%1" %*


from __future__ import print_function
from builtins import input
import sys

print("hello!")
print("using python", sys.version)
input("press RETURN")
