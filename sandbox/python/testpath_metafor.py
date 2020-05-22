#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# teste metafor avec un PATH différent

import os

# print os.environ['PATH']

newpath = []
# newpath.append(r'C:\Program Files (x86)\Intel\VTune\Shared\Bin32e')
# newpath.append(r'C:\Program Files (x86)\Intel\VTune\Analyzer\Bin32e')
# newpath.append(r'C:\Program Files (x86)\Intel\VTune\Analyzer\Bin')
# newpath.append(r'C:\Program Files (x86)\Intel\VTune\Shared\Bin')
# newpath.append(r'C:\Program Files (x86)\Intel\Parallel Studio\Composer\tbb\intel64\vc9\bin')
# newpath.append(r'C:\Program Files (x86)\Intel\Parallel Studio\Composer\ipp\em64t\bin')
# newpath.append(r'C:\Program Files (x86)\Intel\Parallel Studio\Composer\lib\ia32')
# newpath.append(r'd:\lib64\perl\site\bin')
# newpath.append(r'd:\lib64\perl\bin')
newpath.append(r'C:\Windows\system32')
newpath.append(r'C:\Windows')
# newpath.append(r'C:\Windows\System32\Wbem')
# newpath.append(r'C:\Windows\System32\WindowsPowerShell\v1.0')
# newpath.append(r'C:\Program Files (x86)\Druide\Antidote 7\Programmes32')
newpath.append(r'D:\lib64\MKL\10.2.1.019\em64t\bin')
newpath.append(
    r'C:\Program Files (x86)\Intel\Parallel Studio\Composer\lib\intel64')
# newpath.append(r'C:\Program Files (x86)\Xoreax\IncrediBuild')
# newpath.append(r'C:\Program Files\TortoiseSVN\bin')
# newpath.append(r'C:\Program Files (x86)\doxygen\bin')
# newpath.append(r'C:\Program Files (x86)\Common Files\Intel\Shared Files\IDVC')
# newpath.append(r'C:\SIMULIA\Abaqus\Commands')
# newpath.append(r'C:\Program Files\MATLAB\R2009b\bin')
# newpath.append(r'D:\lib64\cmake-2.8.2B\bin\Release')
newpath.append(r'D:\LibsVS2008\Qt\bin')
# newpath.append(r'D:\lib64\gmsh-2.4.2-Windows')
# newpath.append(r'D:\lib64\vtk-5.6.0B\bin\RelWithDebInfo')
# newpath.append(r'D:\lib64\bin')
# newpath.append(r'D:\lib64\vtk-5.6.0B\bin\Debug')
newpath.append(r'D:\lib64\zlib-1.2.4')
# newpath.append(r'D:\lib64\tcltk\bin')
newpath.append(r'D:\lib64\Python-2.6.5\PCbuild\amd64')

newpath.append(r'D:\LibsVS2008\vtk\bin')


# newpath.append(r'C:\SamcefField\V7.2-01_64-bit\SAMCEF-V131\wnt64\bin')
# newpath.append(r'D:\lib64\gnuplot\binary')
# newpath.append(r'C:\Program Files (x86)\VisualSVN Server\bin')
# newpath.append(r'D:\Boman\Bin')

newenv = os.environ
newenv['PATH'] = ';'.join(newpath)

import subprocess
cmd = r'd:\offi\oo_metaI\bin\debug\metafor_d'
cmd = r'd:\offi\oo_metaI\bin\release\metafor'
# p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=fileout, stderr=fileout, env=os.environ, shell=popenShell)
p = subprocess.Popen(cmd, env=newenv, shell=True)
retcode = p.wait()
