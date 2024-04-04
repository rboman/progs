#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from sph import *
from sph.helpers import *

if __name__ == "__main__":

    boxL = 2.
    Lfloor = 0.7
    Lwater = 0.5
    sep = 0.05

    kernel = CubicSplineKernel()

    law = QincFluid()
    law.rho0 = 1000.
    law.gamma = 7.
    law.c0 = 35.

    # parameters
    model = Model()
    model.kernel = kernel
    model.eqState = law
    model.h_0 = 0.06       # initial smoothing length [m]
    model.dom_dim = boxL   # domain size (cube)
    model.alpha = 0.5      # artificial viscosity factor 1
    model.beta = 0.0       # artificial viscosity factor 2
    model.kernelCorrection = False    
    model.maxTime = 1.0    # simulation time
    model.saveInt = 0.01   # save interval

    # fixed particles
    plane = Cube(model, o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, sep),
                 rho=law.rho0, s=sep)
    plane.generate(FixedParticle)
    plane = Cube(model, o=(0, 0, 0),
                 L=(boxL, boxL, sep),
                 rho=law.rho0, s=sep)
    plane.generate(FixedParticle)

    # mobile particles
    cube = Cube(model, o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + 0.5),
                L=(Lwater, Lwater, Lwater),
                rho=law.rho0, s=sep)
    cube.generate(MobileParticle)

    # run SPH model
    runner = Runner(model)
    runner.run()

    # convert to VTK
    try:
        import sph.res2vtp as res2vtp
        res2vtp.ToParaview(verb=False).convertall()
    except Exception as e:
        print("\n**ERROR while converting to VTK:", e)

