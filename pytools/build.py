# -*- coding: utf-8 -*-
#
#   Copyright 2019 Romain Boman
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

"""
Management of CMake/CTest runs for building and testing programs
"""
from __future__ import print_function

import os
import subprocess
import platform


def build():
    """creates a 'build' folder and runs CMake
    """
    # create build dir
    if not os.path.isdir('build'):
        os.mkdir('build')
    os.chdir('build')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call(['cmake', '-A', 'x64', '..'])
    else:
        subprocess.call(['cmake', '..'])
    iop = subprocess.call(['cmake', '--build', '.', '--config', 'Release'])
    if iop != 0:
        raise Exception("cmake FAILED!")
    # go back to where we were
    os.chdir('..')


def test():
    """runs CTest in the 'build' folder
    """
    os.chdir('build')
    # iop = subprocess.call(['ctest', '--verbose', '-C', 'Release'])
    iop = subprocess.call(['ctest', '--output-on-failure', '-C', 'Release'])
    if iop != 0:
        raise Exception("ctest FAILED!")
    os.chdir('..')


def run(progname):
    """runs the compiled program
    """
    os.chdir('build')
    if 'Windows' in platform.uname():
        os.chdir('Release')
    print('running', progname)
    subprocess.check_output([progname])
    os.chdir('..')
