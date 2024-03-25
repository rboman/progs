#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Python helper classes for Louis' code
# Holds the model parameters in a structured way and makes the interface with
# the Fortran executable.

import math, subprocess, os, platform, sys, glob
import sph.wutils as wu


class Kernel:
    """kernel type: contains a "name" (string) and an option "corrected" (bool) 
    enabling the correction near the boundaries.
    """
    names = {'cubic': 1, 'quadratic': 2, 'quintic': 3}

    def __init__(self, name='cubic', corr=True):
        if name in Kernel.names:
            self.name = name
        else:
            raise Exception('unknown kernel kind "%s"' % name)
        self.corrected = corr

    def __str__(self):
        return '%s kernel (%d)' % (self.name, self.val())

    def val(self):
        return Kernel.names[self.name]


class EqState:
    """equation of state: either liquid or gas.
    """
    names = {'gas': 1, 'liquid': 2}

    def __init__(self, name='liquid'):
        if name in EqState.names:
            self.name = name
        else:
            raise Exception('unknown equ of state "%s"' % name)
        self.gamma = 7
        self.molMass = 28.97e-3

    def __str__(self):
        return '%s (%d)' % (self.name, self.val())

    def val(self):
        return EqState.names[self.name]


class Model:
    """A SPH problem with all its parameters.
    """

    def __init__(self):
        # default values of the parameters
        self.h_0 = 0.1          # 3:  [double] initial smoothing length
        self.c_0 = 1480.0       # 4:  [double] initial speed of sound [m/s]
        self.rho_0 = 1000.0     # 5:  [double] initial density [kg/m^3]
        self.dom_dim = 0.0      # 6:  [double] domain size (cube)
        self.kernel = Kernel()  # 7:  [integer] (1:'cubic'/2:'quadratic'/3:'quintic')
        self.alpha = 0.5        # 8:  [double] artificial viscosity factor 1
        self.beta = 0.0         # 9:  [double] artificial viscosity factor 2
        self.law = EqState()    # 10: [integer] (1:'gas'/2:'fluid')
        self.maxTime = 1.0      # 14: [double] simulation time [s]
        self.saveInt = 0.01     # 15: [double] save interval [s]

        # sets of particles
        self.fparts = []
        self.mparts = []

    def writeprm(self):
        """ writes parameters (input.prm)
        """
        file = open('input.prm', 'w')
        file.write('%d\n' % len(self.fparts))
        file.write('%d\n' % len(self.mparts))
        file.write('%e\n' % self.h_0)
        file.write('%e\n' % self.c_0)
        file.write('%e\n' % self.rho_0)
        file.write('%e\n' % self.dom_dim)
        file.write('%d\n' % self.kernel.val())
        file.write('%e\n' % self.alpha)
        file.write('%e\n' % self.beta)
        file.write('%d\n' % self.law.val())
        file.write('%d\n' % self.law.gamma)
        file.write('%e\n' % self.law.molMass)
        file.write('%d\n' % self.kernel.corrected)
        file.write('%e\n' % self.maxTime)
        file.write('%e\n' % self.saveInt)
        file.close()

    def addFixed(self, parts):
        """ adds fixed particles to the model
        """
        self.fparts += parts

    def addMobile(self, parts):
        """ adds mobile particles to the model
        """
        self.mparts += parts

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

        # write parameters
        self.writeprm()

        # "input.mp" - contains mobile particles
        file = open('input.mp', 'w')
        for p in self.mparts:
            p.write(file)
        file.close()

        # "input.fp" - contains fixed particles
        file = open('input.fp', 'w')
        for p in self.fparts:
            p.write(file)
        file.close()

        # "paths.txt" - contains path to the input files
        file = open('paths.txt', 'w')
        file.write('input.prm\n')
        file.write('input.fp\n')
        file.write('input.mp\n')
        file.close()

        # set nb of OpenMP threads
        args = wu.parseargs()
        os.environ['OMP_NUM_THREADS'] = str(args.k)

        # run program
        exename = self.getexe()
        print("running %s using %s threads" % (exename, os.environ['OMP_NUM_THREADS']))

        langprefix = "[F]"
        if args.cpp:
            langprefix = "[C]"
        langprefix = "[exe]"

        # start Fortran code as a subprocess and streams the fortran output
        # to the standard output
        # http://stackoverflow.com/questions/2715847/python-read-streaming-input-from-subprocess-communicate/17698359#17698359
        # try:
        proc = subprocess.Popen(exename, stdout=subprocess.PIPE,
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

    def __str__(self):
        txt = "SPH Model:\n"
        txt += "\tnb fixed particles               = %d\n" % len(self.fparts)
        txt += "\tnb mobile particles              = %d\n" % len(self.mparts)
        txt += "\tinitial smoothing length         = %f\n" % self.h_0
        txt += "\tinitial speed of sound [m/s]     = %f\n" % self.c_0
        txt += "\tinitial density [kg/m^3]         = %f\n" % self.rho_0
        txt += "\tdomain size (cube)               = %f\n" % self.dom_dim
        txt += "\tkernel kind                      = %s\n" % self.kernel
        txt += "\tartificial viscosity factor 1    = %f\n" % self.alpha
        txt += "\tartificial viscosity factor 2    = %f\n" % self.beta
        txt += "\tequ of state (1:'gas'/2:'fluid') = %s\n" % self.law
        txt += "\tfluid prm                        = %d\n" % self.law.gamma
        txt += "\tgas prm [kg/mol]                 = %f\n" % self.law.molMass
        txt += "\tkernel correction (0:no 1:yes)   = %s\n" % self.kernel.corrected
        txt += "\tsimulation time [s]              = %f\n" % self.maxTime
        txt += "\tsave interval [s]                = %f\n" % self.saveInt
        return txt


class Particle:
    """a SPH Particle with position, velocity, pressure, density and mass.
    """

    def __init__(self, x, y, z, vx, vy, vz, rho0, m0):
        self.x = x
        self.y = y
        self.z = z
        self.vx = vx
        self.vy = vy
        self.vz = vz
        self.rho0 = rho0
        self.m0 = m0

    def write(self, file):
        file.write('%e\t%e\t%e\t' % (self.x, self.y, self.z))
        file.write('%e\t%e\t%e\t' % (self.vx, self.vy, self.vz))
        file.write('%e\t%e\n' % (self.rho0, self.m0))

    def __str__(self):
        return "pos=(%f, %f, %f) v=(%f, %f, %f) rho=%f m=%f" % \
            (self.x, self.y, self.z, self.vx, self.vy, self.vz, self.rho0, self.m0)


class Cube:
    """ a basic "cube" defined by its origin (o), size (L), density (rho) and 
    distance between layers (s)
    note: Zero thickness is allowed in any direction.
    """

    def __init__(self, o=(0.0, 0.0, 0.0), L=(1.0, 1.0, 1.0), rho=1.0, s=0.1):
        self.ox = o[0]
        self.oy = o[1]
        self.oz = o[2]
        self.Lx = L[0]
        self.Ly = L[1]
        self.Lz = L[2]
        self.rho = rho
        self.s = s

    def generate(self):
        """returns a list of Particle objects
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
                    p = Particle(x, y, z, vx, vy, vz, rho0, m0)
                    parts.append(p)
        return parts
