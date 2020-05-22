#!/usr/bin/env python3
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

from builtins import range
import os

def main():
    
    #import numpy as np
    #import matplotlib.pyplot as plt
    import ndh

    for i in range(1,9): # run all custom geometries!
        bem = ndh.BemSolver()
        f = os.path.join(os.path.dirname(__file__),'test%d.dat'%i)
        bem.load_data(f)
        bem.exec_full();
        bem.save_Mfile("resfrompy%d.m" % i)



if __name__ == "__main__":
    main()

