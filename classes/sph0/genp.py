#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright 2020 University of Liège
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


import math

class Cube:
    """ a basic "cube" defined by its origin (o), size (L), density (rho) and distance between layers (s)
    """
    def __init__(self, o=(0.0, 0.0, 0.0), L=(1.0, 1.0, 1.0), s=0.1):
        self.ox  = o[0]
        self.oy  = o[1]
        self.oz  = o[2]
        self.Lx  = L[0]
        self.Ly  = L[1]
        self.Lz  = L[2]
        self.s   = s
        
    def generate(self):
        parts = []
        ni = int(math.ceil((self.Lx/self.s)))+1
        dx = self.Lx/ni
        nj = int(math.ceil((self.Ly/self.s)))+1
        dy = self.Ly/nj
        nk = int(math.ceil((self.Lz/self.s)))+1
        dz = self.Lz/nk
        vol = (dx*dy*dz)
        
        for i in range(ni):
            x = self.ox+ i*dx
            for j in range(nj):
                y = self.oy+j*dy
                for k in range(nk):
                    z = self.oz+k*dz
                    p = (x, y, z, vol)
                    parts.append(p)
        return parts
        