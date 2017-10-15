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
    
    import dcm
    plane = dcm.Plane()
    plane.calcule()

    # postprocessing
    
    vp = plane.getValPro()
    print vp
    mp = plane.getModPro(1)  
    print mp
    print plane.getNoPoly()

    import numpy as np
    import matplotlib.pyplot as plt



    xx = np.array(plane.getXX())
    nopoly = plane.getNoPoly()
    for i in range(5):
        mode = np.array(plane.getMODES(i))
        plt.plot(xx, mode, label='mode %d' %(i+1))
   
    plt.xlabel('x')
    #plt.ylim(0,150)
    plt.title('modes')
    plt.grid(True)
    plt.legend()
    plt.show()    

if __name__ == "__main__":
    main()

