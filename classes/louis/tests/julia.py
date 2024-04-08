#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from sph import *
from sph.helpers import *

if __name__ == "__main__":

    boxL = 10.
    Lfloor = 1.2
    Lwater = 0.6
    sep = 0.1

    kernel = CubicSplineKernel()

    law = QincFluid()
    law.rho0 = 1000.
    law.gamma = 7.
    law.c0 = 35.
    
    # parameters
    model = Model()
    model.kernel = kernel
    model.eqState = law
    model.h_0 = sep*1.2    # initial smoothing length [m]
    model.dom_dim = boxL   # domain size (cube)
    model.alpha = 0.5      # artificial viscosity factor 1
    model.beta = 0.0       # artificial viscosity factor 2
    model.kernelCorrection = False    
    model.maxTime = 1.49   # simulation time
    model.saveInt = 0.01   # save interval

    # fixed particles
    plane = Cube(model, o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, 0),
                 rho=law.rho0, s=sep)
    plane.generate(FixedParticle)
    plane = Cube(model, o=(((boxL - Lfloor) / 2) +sep/2, ((boxL - Lfloor) / 2) +sep/2, (boxL / 2) -sep/2),
                 L=(Lfloor -sep, Lfloor -sep, 0),
                 rho=law.rho0, s=sep)
    plane.generate(FixedParticle)

    # mobile particles
    cube = Cube(model, o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + sep),
                L=(Lwater, Lwater, Lwater),
                rho=law.rho0, s=sep)
    cube.generate(MobileParticle)

    # run SPH model
    runner = Runner(model)
    runner.run()
