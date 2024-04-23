#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# "Water in still tank", TFE L. Goffin, p.73

from sph import *
from sph.helpers import *


def model():

    Ldom = 0.8  # computational domain size

    Lbox = 0.7 # box size

    o = (Ldom-Lbox)/2

    Nxy = 35  # nb of particles along x and y
    Nz = 31  # nb of particles along z

    # Nxy = (Lbox - sep) / sep
    # => sep = Lbox/(Nxy+1)

    sep = Lbox/(Nxy+1)

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
    model.dom_dim = Ldom   # domain size (cube)
    model.alpha = 0.5      # artificial viscosity factor 1
    model.beta = 0.0       # artificial viscosity factor 2
    model.kernelCorrection = False    
    model.maxTime = 5.0    # simulation time
    model.saveInt = 0.02   # save interval

    # fixed particles
    # z=0
    plane = Cube(model, o=(o, o, o),
                 L=(boxL, boxL, sep),
                 rho=law.rho0,
                 s=sep)
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
    cube = Cube(model, o=(o, o, o),
                L=(Lbox, 4*L-4*sep, 2*L),
                rho=law.rho0, s=sep)
    cube.generate(MobileParticle)

    # run SPH model
    runner = Runner(model)
    runner.run()


if __name__ == "__main__":
    model()