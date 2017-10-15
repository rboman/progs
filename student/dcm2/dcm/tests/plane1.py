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
    
    import numpy as np
    import matplotlib.pyplot as plt
    import dcm
    plane = dcm.Plane()
    plane.calcule()

    # -- postprocessing --
    
    # traduction mi.m
    x = np.linspace(-plane.enverg, plane.enverg, 101)
    h = (plane.c1-plane.c0)/plane.enverg*np.abs(x)+plane.c0
    l = 10*h
    i = (l*h**3 - (l-2*plane.ep)*(h-2*plane.ep)**3)/12.
    m = plane.densite*2*plane.ep*(11*h-2*plane.ep)

    plt.plot(x,h,label='h(x)')
    plt.plot(x, 1.0e-2 *m, label='m(x)/100')
    plt.plot(x, 200*i, label='200*i(x)')
    plt.grid()
    plt.legend()
    plt.title("Evolution de la masse et de l'inertie")
    plt.show()

    # traduction "mpvp.m"
    vp = plane.getValPro()
    print vp
    mp = plane.getModPro(1)  
    print mp


    print plane.getNoPoly()


    # traduction "graphe.m"
    xx = np.array(plane.getXX())
    #nopoly = plane.getNoPoly()
    for i in range(plane.Nmodes):
        mode = np.array(plane.getMODES(i))
        plt.plot(xx, mode, label='mode %d' %(i+1))
    plt.xlabel('x')
    plt.ylabel('Yi(x)')
    plt.title('Modes propres (normes)')
    plt.grid(True)
    plt.legend()
    plt.show()    

    # traduction "mt.m"
    time = np.linspace(0,plane.Nperiod*plane.T, plane.Nperiod * plane.np2 + 1)
    moment = np.array(plane.getMoment()) 
    tranchant = np.array(plane.getTranchant()) 
    plt.plot(time, moment, label='Moment')
    plt.plot(time, tranchant, label='Tranchant')
    plt.grid(True)
    plt.legend()
    plt.ylabel('M(t) & T(t) en x=0')
    plt.xlabel('temps')
    plt.title('Moment et effort tranchant a l'' emplanture de l'' aile')





    plt.show()

if __name__ == "__main__":
    main()

