#!/usr/bin/env python
# -*- coding: latin-1 -*-
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


def main():

    import sys
    if '--nogui' in sys.argv: return

    import numpy as np
    import matplotlib.pyplot as plt
    import ehd

    code = ehd.Main3()
    code.execute()

    # retrieve results
    h = np.array(code.h) 
    phis = np.array(code.PhiS) 
    phip = np.array(code.PhiP) 
    dphis = np.array(code.dPhiS) 
    dphip = np.array(code.dPhiP) 
    
    # display results
    plt.plot(h, phis, label='phis')
    plt.plot(h, phip, label='phip')    
    plt.plot(h, dphis, label='dphis')
    plt.plot(h, dphip, label='dphip')
    plt.grid(True)
    plt.legend()
    plt.ylabel('Phi')
    plt.xlabel('h')
    plt.title('Patir & Cheng flow factors')
    plt.show()

if __name__ == "__main__":
    main()
