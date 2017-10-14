#! /usr/bin/env python
# -*- coding: latin-1 -*-

def func():
    for i in range(10):
        yield i

a = func() # cree un generateur a partir de la fct "func"
# func= fonction
# func() = generateur

for n in func():
   print n

print "[Enter]"
raw_input()
