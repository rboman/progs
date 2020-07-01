#!/usr/bin/env python3
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

import sys
import platform
system, node, release, version, machine, processor = platform.uname()

machine_name = node.split('.')[0].split('-')[0].lower()
print('machine_name =', machine_name)

print('system =', system)
if system == 'Darwin':
    mac_release, mac_versioninfo, mac_machine = platform.mac_ver()
    print('\tmac_release =', mac_release)
    print('\tmac_versioninfo =', mac_versioninfo)
    print('\tmac_machine =', mac_machine)
if system == 'Linux':
    lin_distname, lin_version, lin_id = platform.linux_distribution()
    print('\tlin_distname =', lin_distname)
    print('\tlin_version =', lin_version)
    print('\tlin_id =', lin_id)
if system == 'Windows':
    win_release, win_version, win_csd, win_ptype = platform.win32_ver()
    print('\twin_release =', win_release)
    print('\twin_version =', win_version)
    print('\twin_csd =', win_csd)
    print('\twin_ptype =', win_ptype)

# --
import subprocess, re


def gcc_version():
    try:
        out = subprocess.check_output(['gcc', '--version'])
    except OSError:
        return 'gcc not found'
    out = out.decode()  # python 3 returns bytes
    m = re.match(r'gcc \(.+\) (\d+\.\d+\.\d+)', out)
    if m and len(m.groups()) > 0:
        return m.group(1)
    else:
        return 'cannot read "gcc --version" output'

def cmake_version():
    try:
        out = subprocess.check_output(['cmake', '--version'])
    except OSError:
        return 'cmake not found'
    out = out.decode()  # python 3 returns bytes
    m = re.match(r'cmake version (\d+\.\d+\.\d+)', out)
    if m and len(m.groups()) > 0:
        return m.group(1)
    else:
        return 'cannot read "cmake --version" output'

def python_version():
    #return sys.version
    return f'{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.minor}'

def numpy_version():
    try:
        import numpy
    except:
        return 'numpy not installed'
    return numpy.version.version

def scipy_version():
    try:
        import scipy
    except:
        return 'scipy not installed'
    return scipy.version.version

def vtk_version():
    try:
        import vtk
    except:
        return 'python-vtk not installed'
    return vtk.vtkVersion.GetVTKVersion()

def qt_version():
    try:
        from PyQt5.QtCore import QT_VERSION_STR
    except:
        return 'PyQt5 not installed'
    return QT_VERSION_STR

def pyqt_version():
    try:
        from PyQt5.Qt import PYQT_VERSION_STR
    except:
        return 'PyQt5 not installed'
    return PYQT_VERSION_STR

def sip_version():
    try:
        from sip import SIP_VERSION_STR
    except:
        return 'sip not installed'
    return SIP_VERSION_STR


if __name__=="__main__":
    print("gcc:", gcc_version())
    print("cmake:", cmake_version())
    print("python:", python_version())
    print("numpy:", numpy_version())
    print("scipy:", numpy_version())
    print("VTK:", vtk_version())
    print("Qt:", qt_version())
    print("PyQt:", pyqt_version())
    print("sip:", sip_version())

