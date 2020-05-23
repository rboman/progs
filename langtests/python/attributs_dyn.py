#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class A(object):
    static = 1

    def __init__(self, x):
        self.x = x
        setattr(self, "double", lambda: 2 * self.x)
        setattr(self, "__add__", lambda y: self.x + y)
        setattr(self, "__mul__", self.__priv)

    def __priv(self, n):
        return self.x * n


a = A(5)
print("a.double() = ", a.double())
print("a+8 = ", a + 8)
print("a*8 = ", a * 8)

print(hasattr(A, "double"))  # False
print(hasattr(a, "double"))  # True
delattr(a, "double")
print(hasattr(a, "double"))  # False
print(dir(a))
print(dir(A))

print("a.static = ", a.static)  # 1
a.static = 2
A.static = 3
print("a.static = ", a.static)  # 2
print("A.static = ", A.static)  # 3
