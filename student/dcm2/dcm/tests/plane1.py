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
    xx = np.array(plane.getXX())
    mode1 = np.array(plane.getMODES(3))

    #print "xx=",xx
    #print "mode1=", mode1

    if 1:
        import matplotlib.pyplot as plt
        plt.plot(xx, mode1)   
        plt.xlabel('x')
        #plt.ylim(0,150)
        plt.title('modes')
        plt.grid(True)
        plt.show()    
    #raw_input()


if __name__ == "__main__":
    main()

