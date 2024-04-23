#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Python helper classes for Louis' code
# Holds the model parameters in a structured way and makes the interface with
# the Fortran executable.

import math, subprocess, os, platform, sys, glob
import sph


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
        # clean prev results
        self.clean()

        args = sph.parseargs()
        if args.cpp:
            self.run_cpp()
        else:
            self.run_fortran()

        # convert results to VTK
        try:
            import sph.res2vtp as res2vtp
            res2vtp.ToParaview(verb=False).convertall()
        except Exception as e:
            print("\n**ERROR while converting to VTK:", e)

    def run_cpp(self):
        """runs a simulation using C++ implementation.
        """
        args = sph.parseargs()
        if args.nosave:
            self.model.nosave = True

        gui = None
        try:
            args = sph.parseargs()
            if not args.nogui:
                gui = sph.QtVTKHook(self.model)
                self.model.set_hook(gui)
        except AttributeError:  # code built without GUI
            gui = None

        self.model.run()

    def run_fortran(self):
        """runs a simulation using Fortran implementation.
        """
        # convertr data to fortran input format
        self.model.to_fortran()

        # setup multithreading for the child process
        args = sph.parseargs()
        os.environ['OMP_NUM_THREADS'] = str(args.k)

        # build command line
        exename = self.getexe()
        print("running %s using %s threads" % (exename, os.environ['OMP_NUM_THREADS']))

        cmd = [exename]

        # add arguments
        # for p in ['--nogui', '--nosave']:
        #     if p in sys.argv:
        #         cmd.append(p)

        langprefix = "[F]"

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

def filled(i, j, k, ni, nj, nk, x, y, z):
    return True

def hollow(i, j, k, ni, nj, nk, x, y, z):
    return i == 0 or i == ni - 1 or j == 0 or j == nj - 1 or k == 0 or k == nk - 1

def hollow_nohat(i, j, k, ni, nj, nk, x, y, z):
    return i == 0 or i == ni - 1 or j == 0 or j == nj - 1 or k == 0

class Sphere:
    def __init__(self, o, r):
        self.cx = o[0]
        self.cy = o[1]
        self.cz = o[2]
        self.r = r
    def inside(self, i, j, k, ni, nj, nk, x, y, z):
        return (x - self.cx) ** 2 + (y - self.cy) ** 2 + (z - self.cz) ** 2 <= self.r ** 2

class Box:
    """ a basic rectangular cuboid defined by its origin (o), size (L), density (rho) and 
    distance between layers (s)
    note: Zero thickness is allowed in any direction (converted to thickness = s).
    """

    def __init__(self, model, o=(0.0, 0.0, 0.0), L=(1.0, 1.0, 1.0), rho=1.0, s=0.1):
        self.model = model
        self.ox = o[0]
        self.oy = o[1]
        self.oz = o[2]
        self.Lx = float(L[0])
        self.Ly = float(L[1])
        self.Lz = float(L[2])
        self.rho = rho
        self.s = s

    def generate(self, ParticleClass, tester=filled):
        """ fills model with Particle objects of type 'ParticleClass'
        """

        # handle planes with "zero" thickness
        Lx = max(self.Lx, self.s)
        Ly = max(self.Ly, self.s)
        Lz = max(self.Lz, self.s)

        ni = round(Lx / self.s)
        dx = Lx / ni
        nj = round(Ly / self.s)
        dy = Ly / nj
        nk = round(Lz / self.s)
        dz = Lz / nk

        vx = vy = vz = 0.0
        m0 = (dx * dy * dz) * self.rho
        rho0 = self.rho

        sx = 0.0 if self.Lx == 0.0 else dx / 2
        sy = 0.0 if self.Ly == 0.0 else dy / 2
        sz = 0.0 if self.Lz == 0.0 else dz / 2
        sx += self.ox
        sy += self.oy
        sz += self.oz

        for i in range(ni):
            x = sx + i * dx
            for j in range(nj):
                y = sy + j * dy
                for k in range(nk):
                    z = sz + k * dz
                    if tester(i, j, k, ni, nj, nk, x, y, z):
                        self.model.add(ParticleClass(x, y, z, vx, vy, vz, rho0, m0))

