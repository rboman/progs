#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from sph import *
from sph.helpers import *


def model(raf_factor=1.0, save_interval=0.01, max_time=1.0, walls=False, shape='cube'):

    boxL = 2.
    Lfloor = 0.7
    Lwater = 0.5
    sep = 0.05 / raf_factor

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
    model.maxTime = max_time    # simulation time
    model.saveInt = save_interval   # save interval

    # fixed particles
    # obstacle
    plane = Box(model, o=(((boxL - Lfloor) / 2), ((boxL - Lfloor) / 2), (boxL / 2)),
                 L=(Lfloor, Lfloor, 0),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle)
    # floor
    plane = Box(model, o=(0, 0, 0),
                 L=(boxL, boxL, 0),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle)

    if walls:
        # x=0
        plane = Box(model, o=(0, 0, 0),
                    L=(0, boxL, boxL),
                    rho=law.rho0,
                    s=sep)
        plane.generate(FixedParticle)
        # y=0
        plane = Box(model, o=(0, 0, 0),
                    L=(boxL, 0, boxL),
                    rho=law.rho0,
                    s=sep)
        plane.generate(FixedParticle)
        # x=L
        plane = Box(model, o=(boxL, 0, 0),
                    L=(0, boxL, boxL),
                    rho=law.rho0,
                    s=sep)
        plane.generate(FixedParticle)
        # y=L
        plane = Box(model, o=(0, boxL, 0),
                    L=(boxL, 0, boxL),
                    rho=law.rho0,
                    s=sep)
        plane.generate(FixedParticle)



    # mobile particles
    cube = Box(model, o=(((boxL - Lwater) / 2), ((boxL - Lwater) / 2), ((boxL) / 2) + 0.5),
                L=(Lwater, Lwater, Lwater),
                rho=law.rho0, s=sep)
    if shape == 'cube':
        cube.generate(MobileParticle)
    elif shape == 'sphere':
        centre = (boxL/2, boxL/2, ((boxL) / 2) + 0.5 + Lwater/2)
        sphere = Sphere(centre, Lwater/2)
        cube.generate(MobileParticle, sphere.inside)

    # run SPH model
    runner = Runner(model)
    runner.run()


if __name__ == "__main__":
    model()