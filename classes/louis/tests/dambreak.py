#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# "Dam break on a dry bed", TFE L. Goffin, p.74
#
#  "Test problem 2: Collapse of a water column", 
#   Boundary Conditions Generated by Dynamic Particles in SPH Methods
#   Cespo-2007
#       https://www.techscience.com/cmc/v5n3/23429/pdf
#
# experiment: Koshizuka and Oka (1996) -
#   "Moving-Particle Semi-Implicit Method for Fragmentation of Incompressible Fluid"
#       http://li.mit.edu/Stuff/CNSE/Paper/Koshizuka96Oka.pdf
# other code:
#   Violeau and Issa (2006)
#   http://www.sciencedirect.com/science/article/pii/S0021999105001963
#

from sph import *
from sph.helpers import *


def model():

    L = 0.5  # caracteristic length of the problem

    boxL = 4*L
    sep = 0.02

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
    model.maxTime = 2.0    # simulation time
    model.saveInt = 0.01   # save interval

    # fixed particles
    plane = Box(model, o=(0, 0, 0),
                 L=(boxL, boxL, boxL),
                 rho=law.rho0,
                 s=sep)
    plane.generate(FixedParticle, hollow_nohat)

    # mobile particles
    cube = Box(model, o=(sep, sep, sep),
                L=(L, 4*L-2*sep, 2*L),
                rho=law.rho0, s=sep)
    cube.generate(MobileParticle)

    # run SPH model
    runner = Runner(model)
    runner.run()


if __name__ == "__main__":
    model()