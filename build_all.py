#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os


basedir = os.path.abspath(os.path.dirname(__file__))
#print 'basedir=', basedir

os.chdir(os.path.join(basedir,'apps','EHD'))
execfile('build.py')

os.chdir(os.path.join(basedir,'apps','fractal', 'cpp'))
execfile('build.py')

os.chdir(os.path.join(basedir,'apps','GenMAI'))
execfile('build.py')

os.chdir(os.path.join(basedir,'apps','md5'))
execfile('build.py')

os.chdir(os.path.join(basedir,'apps','minibarreTE')) # requires gmm
execfile('build.py')

# os.chdir(os.path.join(basedir,'student','dcm1'))   # requires Qt
# execfile('build.py')

os.chdir(os.path.join(basedir,'student','dcm2'))
execfile('build.py')

os.chdir(os.path.join(basedir,'student','ndh'))
execfile('build.py')



