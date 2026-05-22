# -*- coding: utf-8 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

import math
from dataclasses import dataclass

@dataclass
class Point:
    x: float
    y: float

    def __str__(self):
        return "(%f,%f)" % (self.x, self.y)


class Curve:
    def __init__(self):
        self.pts = []

    def fill(self, f, rng, n):
        x = float(rng[0])
        dx = (float(rng[1]) - float(rng[0])) / n
        for i in range(n):
            self.pts.append(Point(x, f(x)))
            x += dx

    def __str__(self):
        s = ""
        for p in self.pts:
            s += str(p) + " "
        return s

    def __iter__(self):
        for p in self.pts:
            yield p
