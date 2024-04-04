#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from sph import *
from sph.helpers import *

if __name__ == "__main__":

    boxL = 2.
    Lfloor = 0.7
    Lwater = 0.5
    sep = 0.05

    kernelpp = CubicSplineKernel()

    lawpp = QincFluid()
    lawpp.rho0 = 1000.
    lawpp.gamma = 7.
    lawpp.c0 = 35.

    # parameters
    modelpp = Model()
    modelpp.kernel = kernelpp
    modelpp.eqState = lawpp
    modelpp.h_0 = 0.06        # initial smoothing length [m]
    modelpp.dom_dim = boxL    # domain size (cube)
    modelpp.alpha = 0.5       # artificial viscosity factor 1
    modelpp.beta = 0.0        # artificial viscosity factor 2
    modelpp.kernelCorrection = False 
    modelpp.maxTime = 3.0     # simulation time
    modelpp.saveInt = 0.01    # save interval

    # fixed particles
    # obstacle
    plane = Cube(modelpp, o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, sep),
                 rho=lawpp.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # floor
    plane = Cube(modelpp, o=(0, 0, 0),
                 L=(boxL, boxL, sep),
                 rho=lawpp.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # x=0
    plane = Cube(modelpp, o=(0, 0, 2 * sep),
                 L=(sep, boxL, boxL - 2 * sep),
                 rho=lawpp.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # y=0
    plane = Cube(modelpp, o=(2 * sep, 0, 2 * sep),
                 L=(boxL - 4 * sep, sep, boxL - 2 * sep),
                 rho=lawpp.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # x=L
    plane = Cube(modelpp, o=(boxL - sep, 0, 2 * sep),
                 L=(sep, boxL, boxL - 2 * sep),
                 rho=lawpp.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # y=L
    plane = Cube(modelpp, o=(2 * sep, boxL - sep, 2 * sep),
                 L=(boxL - 4 * sep, sep, boxL - 2 * sep),
                 rho=lawpp.rho0,
                 s=sep)
    plane.generate(FixedParticle)

    # mobile particles
    cube = Cube(modelpp, o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + 0.5),
                L=(Lwater, Lwater, Lwater),
                rho=lawpp.rho0,
                s=sep)
    cube.generate(MobileParticle)

    # run SPH model
    model = hModel(modelpp)
    model.run()

    # convert to VTK
    try:
        import sph.res2vtp as res2vtp
        res2vtp.ToParaview(verb=False).convertall()
    except Exception as e:
        print("\n**ERROR while converting to VTK:", e)
