#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright 2020 University of Liège
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# Python helper classes for Louis' code

import math, subprocess, os, platform, sys, glob
import sph.wutils as wu

class Kernel:
    names = { 'cubic':1, 'quadratic':2, 'quintic':3 }
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
    names = { 'gas':1, 'liquid':2 }
    def __init__(self, name='liquid'):
        if name in EqState.names:
            self.name = name
        else:
            raise Exception('unknown equ of state "%s"' % name)
        self.gamma   = 7
        self.molMass = 28.97e-3
    def __str__(self):
        return '%s (%d)' % (self.name, self.val())
    def val(self):
        return EqState.names[self.name]    


class Model:
    def __init__(self):
        # default values of the parameters
        self.h_0              = 0.1         # 3:  [double] initial smoothing length
        self.c_0              = 1480.0      # 4:  [double] initial speed of sound [m/s]
        self.rho_0            = 1000.0      # 5:  [double] initial density [kg/m^3]
        self.dom_dim          = 0.0         # 6:  [double] domain size (cube)
        self.kernel           = Kernel()    # 7:  [integer] (1:'cubic'/2:'quadratic'/3:'quintic')
        self.alpha            = 0.5         # 8:  [double] artificial viscosity factor 1
        self.beta             = 0.0         # 9:  [double] artificial viscosity factor 2
        self.law              = EqState()   # 10: [integer] (1:'gas'/2:'fluid')
        self.maxTime          = 1.0         # 14: [double] simulation time [s]
        self.saveInt          = 0.01        # 15: [double] save interval [s]
        
        # sets of particles
        self.fparts = []
        self.mparts = []
        
    def writeprm(self):
        """ writes parameters (input.prm)
        """
        file = open('input.prm','w')
        file.write('%d\n' % len(self.fparts) )
        file.write('%d\n' % len(self.mparts) )        
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
        for p in ['res*.*', 'input.*', 'grid.*']:
            for f in glob.glob(p):
                print('rm %s' % f)
                os.remove(f)

    def run(self):
    
        # clean prev results
        self.clean()
        
        # write parameters
        self.writeprm()
    
        # input.mp - contains mobile particles
        file = open('input.mp','w')
        for p in self.mparts:
            p.write(file)
        file.close()    

        # input.fp - contains fixed particles
        file = open('input.fp','w')
        for p in self.fparts:
            p.write(file)
        file.close()    

        # paths.txt - contains path to the input files
        file = open('paths.txt','w')
        file.write('input.prm\n')
        file.write('input.fp\n')
        file.write('input.mp\n')
        file.close()
        
        # set nb of threads
        args = wu.parseargs()
        os.environ['OMP_NUM_THREADS'] = str(args.k)
       
        # run program
        exename = self.getexe()
        print("running %s using %s threads" % (exename, os.environ['OMP_NUM_THREADS']))
        
        #iop = subprocess.call(self.getexe(), shell=True, stdout=sys.stdout, stderr=sys.stderr)
        #print 'louis returned iop=%d', iop

        #http://stackoverflow.com/questions/2715847/python-read-streaming-input-from-subprocess-communicate/17698359#17698359
        
        #file=open('pipo.txt','w')
        try:
            proc = subprocess.Popen(exename, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, bufsize=1)
            with proc.stdout:
                for line in iter(proc.stdout.readline, b''):
                    line = line.rstrip('\n').rstrip('\r')
                    print('[F]%s' % line)
                    #sys.stdout.write(line)
                    #file.write("line=\"%s\"\n" % line)
            proc.wait()
        except KeyboardInterrupt:
            print('Ignoring CTRL-C')
            pass
        #file.close()
        
    def getexe(self):
        """ looks for fortran exe
        """
        dir1=os.path.abspath(os.path.dirname(__file__)+os.sep+".."+os.sep+"..")+os.sep+"louisB"
        if 'Windows' in platform.uname():
            exename = os.path.join(dir1, "Release/louis.exe")
        else:
            exename = os.path.join(dir1, "louis")
        if not os.path.isfile(exename):
            raise Exception ("%s NOT found" %exename)
        return exename          
        
    def __str__(self):
        txt = "SPH Model:\n"
        txt += "\tnb fixed particiles              = %d\n" % len(self.fparts)          
        txt += "\tnb mobile particules             = %d\n" % len(self.mparts)
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
    def __init__(self, x,y,z, vx,vy,vz, rho0, m0):
        self.x = x
        self.y = y
        self.z = z
        self.vx = vx
        self.vy = vz
        self.vz = vz
        self.rho0 = rho0
        self.m0 = m0
    def write(self, file):
        file.write('%e\t%e\t%e\t' % (self.x,self.y,self.z))
        file.write('%e\t%e\t%e\t' % (self.vx,self.vy,self.vz))
        file.write('%e\t%e\n' % (self.rho0, self.m0))   
    def __str__(self):
        return "pos=(%f, %f, %f) v=(%f, %f, %f) rho=%f m=%f" % \
        (self.x, self.y, self.z,  self.vx, self.vy, self.vz, self.rho0, self.m0)     
        
class Cube:
    """ a basic "cube" defined by its origin (o), size (L), density (rho) and distance between layers (s)
    """
    def __init__(self, o=(0.0, 0.0, 0.0), L=(1.0, 1.0, 1.0), rho=1.0, s=0.1):
        self.ox  = o[0]
        self.oy  = o[1]
        self.oz  = o[2]
        self.Lx  = L[0]
        self.Ly  = L[1]
        self.Lz  = L[2]
        self.rho = rho
        self.s   = s
        
    def generate(self):
        parts = []
        ni = int(math.ceil((self.Lx/self.s)))+1
        dx = self.Lx/(ni-1)
        nj = int(math.ceil((self.Ly/self.s)))+1
        dy = self.Ly/(nj-1)
        nk = int(math.ceil((self.Lz/self.s)))+1
        dz = self.Lz/(nk-1)
        vx = vy = vz = 0.0
        m0 = (dx*dy*dz)*self.rho
        rho0 = self.rho
        
        for i in range(ni):
            x = self.ox+ i*dx
            for j in range(nj):
                y = self.oy+j*dy
                for k in range(nk):
                    z = self.oz+k*dz
                    p = Particle(x, y, z, vx, vy, vz, rho0, m0)
                    parts.append(p)
        return parts
        

    
    
    
    

    