#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This example uses trimesh to load a simple CAD model
# and fill it with particles
# see https://trimesh.org/


from sph import *
from sph.helpers import *


def model():

    boxL = 2.  # box size

    sep = 0.03

    kernel = CubicSplineKernel()

    law = QincFluid()
    law.rho0 = 1000.
    law.gamma = 7.
    law.c0 = 35.

    # parameters
    model = Model()
    model.kernel = kernel
    model.eqState = law
    model.h_0 = sep * 1.2    # initial smoothing length [m]
    model.dom_dim = boxL   # domain size (cube)
    model.alpha = 0.5      # artificial viscosity factor 1
    model.beta = 0.0       # artificial viscosity factor 2
    model.kernelCorrection = False
    model.maxTime = 3.0    # simulation time
    model.saveInt = 0.02   # save interval

    # fixed particles
    plane = Box(model, o=(0, 0, 0),
                L=(boxL, boxL, boxL),
                rho=law.rho0,
                s=sep)
    plane.generate(FixedParticle, hollow)

    import numpy as np
    import trimesh
    # trimesh.util.attach_to_log()

    thisdir = os.path.dirname(__file__)
    objfile = os.path.join(thisdir, "tetrahedron.obj")
    mesh = trimesh.load_mesh(objfile)
    # mesh.apply_translation(np.array([0.4, 0.4, 0.4]) * boxL)

    origin, xaxis, yaxis, zaxis = [0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1]
    S = trimesh.transformations.scale_matrix(1.2, origin)
    T = trimesh.transformations.translation_matrix(np.array([0.3, 0.3, 0.5]) * boxL)
    R = trimesh.transformations.rotation_matrix(np.pi / 4, yaxis)
    M = trimesh.transformations.concatenate_matrices(T, R, S)
    # mesh.apply_transform(trimesh.transformations.rotation_matrix(np.pi/4, [1, 0, 0]))
    mesh.apply_transform(M)

    def inside_obj(i, j, k, ni, nj, nk, x, y, z):
        return mesh.contains(np.array([[x, y, z]]))

    # mobile particles
    cube = Box(model, o=(sep, sep, sep),
               L=(boxL - 2 * sep, boxL - 2 * sep, boxL - 2 * sep),
               rho=law.rho0, s=sep)
    cube.generate(MobileParticle, inside_obj)

    # run SPH model
    runner = Runner(model)
    runner.run()


if __name__ == "__main__":
    model()
