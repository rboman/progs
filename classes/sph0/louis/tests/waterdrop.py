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


from sph.helpers import *

if __name__=="__main__":

    boxL   = 2.
    Lfloor = 0.7
    Lwater = 0.5
    sep = 0.05

    kernel = Kernel('cubic', True)    # 'cubic', 'quadratic' or 'quintic'
    law = EqState('liquid')           # 'gas' or 'liquid'
    
    # parameters
    model = Model()
    model.kernel = kernel     
    model.law    = law     
    
    model.h_0      = 0.06       # initial smoothing length [m]
    model.c_0      = 35.0       # initial speed of sound  [m/s]
    model.rho_0    = 1000.0     # initial density [kg/m^3]
    model.dom_dim  = boxL       # domain size (cube)
    model.alpha    = 0.5        # artificial viscosity factor 1
    model.beta     = 0.0        # artificial viscosity factor 2
    model.maxTime  = 1.0       # simulation time
    model.saveInt  = 0.01       # save interval
    
    # mobile particles
    cube = Cube( o=(((boxL-Lwater)/2),((boxL-Lwater)/2), ((boxL)/2)+0.5), L=(Lwater,Lwater,Lwater), rho=model.rho_0, s=sep)
    model.addMobile(cube.generate())
    
    # fixed particles
    plane = Cube( o=(((boxL-Lfloor)/2),((boxL-Lfloor)/2), (boxL/2)), L=(Lfloor,Lfloor,sep), rho=model.rho_0, s=sep)
    model.addFixed(plane.generate())
    plane = Cube( o=(0,0,0), L=(boxL,boxL,sep), rho=model.rho_0, s=sep)
    model.addFixed(plane.generate())
    
    # run SPH model
    print(model)
    model.run()    
  
    # convert to VTK
    import sph.gui as gui
    gui.ToParaview(verb=False).convertall()

    