#!/usr/bin/env python3
# -*- coding: utf-8 -*-


class Pt:
    """ A simple point class
    """

    def __init__(self, x, y, z):
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)

    def __str__(self):
        return '(' + str(self.x) + ',' + str(self.y) + ',' + str(self.z) + ')'

    def __add__(self, pt):
        return Pt(self.x+pt.x, self.y+pt.y, self.z+pt.z)

    def __sub__(self, pt):
        return Pt(self.x-pt.x, self.y-pt.y, self.z-pt.z)

    def __div__(self, scalar):
        return Pt(self.x//scalar, self.y//scalar, self.z//scalar)

    def __truediv__(self, scalar):
        return Pt(self.x/scalar, self.y/scalar, self.z/scalar)

    def __mul__(self, obj):
        if isinstance(obj, Pt):
            return self.x*obj.x + self.y*obj.y + self.z*obj.z
        else:
            return Pt(self.x*obj, self.y*obj, self.z*obj)

    def __rmul__(self, scalar):
        return self.__mul__(scalar)

    def __abs__(self):
        return math.sqrt(self*self)

    def normalize(self):
        return self/abs(self)

    def min(self, obj):
        if obj.x < self.x:
            self.x = obj.x
        if obj.y < self.y:
            self.y = obj.y
        if obj.z < self.z:
            self.z = obj.z

    def max(self, obj):
        if obj.x > self.x:
            self.x = obj.x
        if obj.y > self.y:
            self.y = obj.y
        if obj.z > self.z:
            self.z = obj.z

    def copy(self):
        return Pt(self.x, self.y, self.z)

    def __gt__(self, obj):
        return (self.x > obj.x and self.y > obj.y and self.z > obj.z)

    def __lt__(self, obj):
        return (self.x < obj.x and self.y < obj.y and self.z < obj.z)

    def __ge__(self, obj):
        return (self.x >= obj.x and self.y >= obj.y and self.z >= obj.z)

    def __le__(self, obj):
        return (self.x <= obj.x and self.y <= obj.y and self.z <= obj.z)


if __name__ == "__main__":
    p1 = Pt(1, 1, 1)
    p2 = Pt(1, 2, 3)
    print(f'p1={p1}')
    print(f'p2={p2}')
    print(f'p1/2+p2={p1/2+p2}')
    print(f'p1-2*p2={p1-2*p2}')
