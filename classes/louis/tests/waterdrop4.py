#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from sph.helpers import *

if __name__ == "__main__":

    boxL = 2.
    Lfloor = 0.7
    Lwater = 0.5
    sep = 0.05 / 4

    kernel = Kernel('cubic', False)   # 'cubic', 'quadratic' or 'quintic'
    law = EqState('liquid')           # 'gas' or 'liquid'

    # parameters
    model = Model()
    model.kernel = kernel
    model.law = law

    model.h_0 = 0.06 / 4       # initial smoothing length [m]
    model.c_0 = 35.0           # initial speed of sound  [m/s]
    model.rho_0 = 1000.0       # initial density [kg/m^3]
    model.dom_dim = boxL       # domain size (cube)
    model.alpha = 0.5          # artificial viscosity factor 1
    model.beta = 0.0           # artificial viscosity factor 2
    model.maxTime = 3.0        # simulation time
    model.saveInt = 0.01 / 2   # save interval

    # mobile particles
    cube = Cube(o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + 0.5),
                L=(Lwater, Lwater, Lwater),
                rho=model.rho_0,
                s=sep)
    model.addMobile(cube.generate())

    # fixed particles
    # obstacle
    plane = Cube(o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, sep),
                 rho=model.rho_0,
                 s=sep)
    model.addFixed(plane.generate())
    # floor
    plane = Cube(o=(0, 0, 0), L=(boxL, boxL, sep),
                 rho=model.rho_0,
                 s=sep)
    model.addFixed(plane.generate())
    # x=0
    plane = Cube(o=(0, 0, 2 * sep),
                 L=(sep, boxL, boxL - 2 * sep),
                 rho=model.rho_0,
                 s=sep)
    model.addFixed(plane.generate())
    # y=0
    plane = Cube(o=(2 * sep, 0, 2 * sep),
                 L=(boxL - 4 * sep, sep, boxL - 2 * sep),
                 rho=model.rho_0,
                 s=sep)
    model.addFixed(plane.generate())
    # x=L
    plane = Cube(o=(boxL - sep, 0, 2 * sep),
                 L=(sep, boxL, boxL - 2 * sep),
                 rho=model.rho_0, s=sep)
    model.addFixed(plane.generate())
    # y=L
    plane = Cube(o=(2 * sep, boxL - sep, 2 * sep),
                 L=(boxL - 4 * sep, sep, boxL - 2 * sep),
                 rho=model.rho_0,
                 s=sep)
    model.addFixed(plane.generate())

    # run SPH model
    print(model)
    model.run()

    # convert to VTK
    try:
        import sph.res2vtp as res2vtp
        res2vtp.ToParaview(verb=False).convertall()
    except Exception as e:
        print("\n**ERROR while converting to VTK:", e)
