#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from sph.helpers import *

if __name__ == "__main__":

    boxL = 10.
    Lfloor = 1.2
    Lwater = 0.6
    sep = 0.1

    kernel = Kernel('cubic', False)   # 'cubic', 'quadratic' or 'quintic'
    law = EqState('liquid')           # 'gas' or 'liquid'

    # parameters
    model = Model()
    model.kernel = kernel
    model.law = law

    model.h_0 = sep*1.2    # initial smoothing length [m]
    model.c_0 = 35.0       # initial speed of sound  [m/s]
    model.rho_0 = 1000.0   # initial density [kg/m^3]
    model.dom_dim = boxL   # domain size (cube)
    model.alpha = 0.5      # artificial viscosity factor 1
    model.beta = 0.0       # artificial viscosity factor 2
    model.maxTime = 1.49    # simulation time
    model.saveInt = 0.01   # save interval

    # mobile particles
    cube = Cube(o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + sep),
                L=(Lwater, Lwater, Lwater),
                rho=model.rho_0, s=sep)
    model.addMobile(cube.generate())

    # fixed particles
    plane = Cube(o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, 0),
                 rho=model.rho_0, s=sep)
    model.addFixed(plane.generate())
    plane = Cube(o=(((boxL - Lfloor) / 2) +sep/2, ((boxL - Lfloor) / 2) +sep/2, (boxL / 2) -sep/2),
                 L=(Lfloor -sep, Lfloor -sep, 0),
                 rho=model.rho_0, s=sep)
    model.addFixed(plane.generate())

    # run SPH model
    print(model)
    model.run()

    # convert to VTK
    try:
        print("Converting to VTK")
        import sph.gui as gui
        gui.ToParaview(verb=False).convertall()
    except Exception as e:
        print("\n**ERROR while converting to VTK:", e)
