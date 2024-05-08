#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# "Water in still tank", TFE L. Goffin, p.73

from sph import *
from sph.helpers import *


def model():

    boxL = 0.7 # box size

    fluidH = 0.62

    Nxy = 35  # nb of particles along x and y
    # Nz = 31  # nb of particles along z

    # Nxy = (boxL - sep) / sep
    sep = boxL/(Nxy+1)

    kernel = CubicSplineKernel()

    law = QincFluid()
    law.rho0 = 1000.
    law.gamma = 7.
    law.c0 = 30.

    # parameters
    model = Model()
    model.kernel = kernel
    model.eqState = law
    model.h_0 = sep*1.2    # initial smoothing length [m]
    model.dom_dim = boxL   # domain size (cube)
    model.alpha = 0.05     # artificial viscosity factor 1
    model.beta = 0.0       # artificial viscosity factor 2
    model.kernelCorrection = False    
    model.maxTime = 5.0    # simulation time
    model.saveInt = 0.02   # save interval

    # fixed particles
    plane = Box(model, o=(0, 0, 0),
                 L=(boxL, boxL, boxL),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle, hollow_nohat)

    # mobile particles
    cube = Box(model, o=(sep, sep, sep),
                L=(boxL-2*sep, boxL-2*sep, fluidH),
                rho=law.rho0, s=sep)
    cube.generate(MobileParticle)

    # run SPH model
    runner = Runner(model)
    runner.run()


if __name__ == "__main__":
    model()