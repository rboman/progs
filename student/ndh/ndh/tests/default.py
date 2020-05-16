#!/usr/bin/env python
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

import matplotlib
matplotlib.use('Qt5Agg')  # force PyQt5

def main():
    
    import numpy as np
    import ndh
    bem = ndh.BemSolver()
    bem.exec_full();
    bem.save_Mfile("resfrompy.m")



    sol = bem.getSolution()

    import matplotlib.pyplot as plt
    plt.plot(sol, label='solution')
    plt.xlabel('to be fixed')
    plt.ylabel('Temperature')
    plt.title('Solution')
    plt.grid(True)
    plt.legend()
    plt.show() 


if __name__ == "__main__":
    main()

