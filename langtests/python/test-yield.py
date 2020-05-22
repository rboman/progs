#! /usr/bin/env python
# -*- coding: utf-8 -*-


from __future__ import print_function
from builtins import input
from builtins import range
def func():
    for i in range(10):
        yield i


a = func()  # cree un generateur a partir de la fct "func"
# func= fonction
# func() = generateur

for n in func():
    print(n)

print("[Enter]")
input()
