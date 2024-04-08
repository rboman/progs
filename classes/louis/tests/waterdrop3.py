#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from sph import *
from sph.helpers import *

if __name__ == "__main__":

    boxL = 2.
    Lfloor = 0.7
    Lwater = 0.5
    sep = 0.05 / 2

    kernel = CubicSplineKernel()

    law = QincFluid()
    law.rho0 = 1000.
    law.gamma = 7.
    law.c0 = 35.

    # parameters
    model = Model()
    model.kernel = kernel
    model.eqState = law
    model.h_0 = 0.06 / 2       # initial smoothing length [m]
    model.dom_dim = boxL       # domain size (cube)
    model.alpha = 0.5          # artificial viscosity factor 1
    model.beta = 0.0           # artificial viscosity factor 2
    model.kernelCorrection = False 
    model.maxTime = 3.0        # simulation time
    model.saveInt = 0.01 / 2   # save interval

    # fixed particles
    # obstacle
    plane = Cube(model, o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, sep),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # floor
    plane = Cube(model, o=(0, 0, 0),
                 L=(boxL, boxL, sep),
                 rho=law.rho0, s=sep)
    plane.generate(FixedParticle)
    # x=0
    plane = Cube(model, o=(0, 0, 2 * sep),
                 L=(sep, boxL, boxL - 2 * sep),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # y=0
    plane = Cube(model, o=(2 * sep, 0, 2 * sep),
                 L=(boxL - 4 * sep, sep, boxL - 2 * sep),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # x=L
    plane = Cube(model, o=(boxL - sep, 0, 2 * sep),
                 L=(sep, boxL, boxL - 2 * sep),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # y=L
    plane = Cube(model, o=(2 * sep, boxL - sep, 2 * sep),
                 L=(boxL - 4 * sep, sep, boxL - 2 * sep),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle)

    # mobile particles
    cube = Cube(model, o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + 0.5),
                L=(Lwater, Lwater, Lwater),
                rho=law.rho0,
                s=sep)
    cube.generate(MobileParticle)

    # run SPH model
    runner = Runner(model)
    runner.run()
