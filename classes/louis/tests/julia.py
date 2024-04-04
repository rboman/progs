#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from sph import *
from sph.helpers import *

if __name__ == "__main__":

    boxL = 10.
    Lfloor = 1.2
    Lwater = 0.6
    sep = 0.1

    kernelpp = CubicSplineKernel()

    lawpp = QincFluid()
    lawpp.rho0 = 1000.
    lawpp.gamma = 7.
    lawpp.c0 = 35.
    
    # parameters
    modelpp = Model()
    modelpp.kernel = kernelpp
    modelpp.eqState = lawpp
    modelpp.h_0 = sep*1.2    # initial smoothing length [m]
    modelpp.dom_dim = boxL   # domain size (cube)
    modelpp.alpha = 0.5      # artificial viscosity factor 1
    modelpp.beta = 0.0       # artificial viscosity factor 2
    modelpp.kernelCorrection = False    
    modelpp.maxTime = 1.49    # simulation time
    modelpp.saveInt = 0.01   # save interval

    # fixed particles
    plane = Cube(modelpp, o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, 0),
                 rho=lawpp.rho0, s=sep)
    plane.generate(FixedParticle)
    plane = Cube(modelpp, o=(((boxL - Lfloor) / 2) +sep/2, ((boxL - Lfloor) / 2) +sep/2, (boxL / 2) -sep/2),
                 L=(Lfloor -sep, Lfloor -sep, 0),
                 rho=lawpp.rho0, s=sep)
    plane.generate(FixedParticle)


    # mobile particles
    cube = Cube(modelpp, o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + sep),
                L=(Lwater, Lwater, Lwater),
                rho=lawpp.rho0, s=sep)
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
        