#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Python helper classes for Louis' code
# Holds the model parameters in a structured way and makes the interface with
# the Fortran executable.

import math, subprocess, os, platform, sys, glob
import sph.wutils as wu

class Runner:
    """A SPH problem with all its parameters.
    """

    def __init__(self, model):
        self.model = model

    def clean(self):
        """cleans files from workspace
        """
        for p in ['res*.*', 'input.*', 'grid.*']:
            for f in glob.glob(p):
                # print('rm %s' % f)
                os.remove(f)

    def run(self):
        """runs a simulation with the given set of parameters and particles.
        """
        # clean prev results
        self.clean()

        self.model.to_fortran()   # uniqt si Fortran

        # set nb of OpenMP threads
        args = wu.parseargs()
        os.environ['OMP_NUM_THREADS'] = str(args.k)

        # build command line
        exename = self.getexe()
        print("running %s using %s threads" % (exename, os.environ['OMP_NUM_THREADS']))

        cmd = [ exename ]

        # add arguments
        for p in ['--nogui', '--nosave']:
            if p in sys.argv:
                cmd.append(p)

        langprefix = "[F]"
        if args.cpp:
            langprefix = "[C]"
        langprefix = "[exe]"

        # start Fortran code as a subprocess and streams the fortran output
        # to the standard output
        # http://stackoverflow.com/questions/2715847/python-read-streaming-input-from-subprocess-communicate/17698359#17698359
        # try:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        with proc.stdout:
            for line in iter(proc.stdout.readline, b''):
                line = line.decode().rstrip('\n').rstrip('\r')
                print(f'{langprefix}{line}')
        proc.wait()
        # except KeyboardInterrupt:
        #     print('Ignoring CTRL-C')
        #     pass

    def getexe(self):
        """ looks for Louis' executable
        """
        args = wu.parseargs()
        if args.cpp:
            exename = "louis++"
        else:
            exename = "louis"
        if 'Windows' in platform.uname():
            exename += ".exe"
        dir1 = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "build", "bin"))
        paths = [os.path.join(dir1, exename),
                 os.path.join(dir1, "Release", exename),
                 os.path.join(dir1, "Debug", exename)]

        for p in paths:
            if os.path.isfile(p):
                exename = p
                break
        else:
            raise Exception("%s NOT found" % exename)
        return exename

    # def __str__(self):
    #     txt = "SPH Model:\n"
    #     txt += "\tnb fixed particles               = %d\n" % len(self.fparts)
    #     txt += "\tnb mobile particles              = %d\n" % len(self.mparts)
    #     txt += "\tinitial smoothing length         = %f\n" % self.h_0
    #     txt += "\tinitial speed of sound [m/s]     = %f\n" % self.c_0
    #     txt += "\tinitial density [kg/m^3]         = %f\n" % self.rho_0
    #     txt += "\tdomain size (cube)               = %f\n" % self.dom_dim
    #     txt += "\tkernel kind                      = %s\n" % self.kernel
    #     txt += "\tartificial viscosity factor 1    = %f\n" % self.alpha
    #     txt += "\tartificial viscosity factor 2    = %f\n" % self.beta
    #     txt += "\tequ of state (1:'gas'/2:'fluid') = %s\n" % self.law
    #     txt += "\tfluid prm                        = %d\n" % self.law.gamma
    #     txt += "\tgas prm [kg/mol]                 = %f\n" % self.law.molMass
    #     txt += "\tkernel correction (0:no 1:yes)   = %s\n" % self.kernel.corrected
    #     txt += "\tsimulation time [s]              = %f\n" % self.maxTime
    #     txt += "\tsave interval [s]                = %f\n" % self.saveInt
    #     return txt

class Cube:
    """ a basic "cube" defined by its origin (o), size (L), density (rho) and 
    distance between layers (s)
    note: Zero thickness is allowed in any direction.
    """

    def __init__(self, model, o=(0.0, 0.0, 0.0), L=(1.0, 1.0, 1.0), rho=1.0, s=0.1):
        self.model = model
        self.ox = o[0]
        self.oy = o[1]
        self.oz = o[2]
        self.Lx = L[0]
        self.Ly = L[1]
        self.Lz = L[2]
        self.rho = rho
        self.s = s

    def generate(self, ParticleClass):
        """ fills model with Particle objects of type 'ParticleClass'
        """
        parts = []
        ni = int(math.ceil((self.Lx / self.s))) + 1
        dx = 0.0 if ni==1 else self.Lx / (ni - 1)
        nj = int(math.ceil((self.Ly / self.s))) + 1
        dy = 0.0 if nj==1 else self.Ly / (nj - 1)
        nk = int(math.ceil((self.Lz / self.s))) + 1
        dz = 0.0 if nk==1 else self.Lz / (nk - 1)

        dvx = self.s if ni==1 else dx
        dvy = self.s if nj==1 else dy
        dvz = self.s if nk==1 else dz

        vx = vy = vz = 0.0
        m0 = (dvx * dvy * dvz) * self.rho
        rho0 = self.rho

        for i in range(ni):
            x = self.ox + i * dx
            for j in range(nj):
                y = self.oy + j * dy
                for k in range(nk):
                    z = self.oz + k * dz
                    self.model.add(ParticleClass(x, y, z, vx, vy, vz, rho0, m0))
