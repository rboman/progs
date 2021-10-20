#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2017-2021 Romain Boman
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
Script used to quickly update & build Metafor and keygen on any system
This should work on Windows, Linux and macOS.

The script tries to guess whether the source code is up-to-date and the 
executables should be rebuild.
Then it pulls changes from the remote repo (this could cause conflicts)
and runs the build procedure in parallel using incredibuild on Windows.

Run using: "rb.py updateoffi.py"
"""

import os
import shutil
import platform
import subprocess
import multiprocessing
import pytools.utils as pu
import pytools.versioning as vrs


def guessSystem():
    """try to guess the names of the cmake configuration files of Metafor
    that could be used for the current machine according to OS type, hostname, etc.
    Returns a list of guesses in 'guesses'  
    """
    guesses = []
    system, node, release, version, machine, processor = platform.uname()

    # machine name
    machine_name = node.split('.')[0].split('-')[0].lower()
    print('machine_name =', machine_name)
    guesses.append(machine_name)

    # robo's libs
    # print os.environ['MYLOCAL']
    if 'MYLOCAL' in os.environ:
        guesses.append('garfield')

    # system name
    print('system =', system)
    if system == 'Darwin':
        mac_release, mac_versioninfo, mac_machine = platform.mac_ver()
        print('\tmac_release =', mac_release)
        print('\tmac_versioninfo =', mac_versioninfo)
        print('\tmac_machine =', mac_machine)
        guesses.append('macos')
    if system == 'Linux':
        if sys.version_info.minor >= 8:
            import distro as m
        else:
            import platform as m
        lin_distname, lin_version, lin_id = m.linux_distribution()
        print('\tlin_distname =', lin_distname)
        print('\tlin_version =', lin_version)
        print('\tlin_id =', lin_id)
        guesses.append(lin_distname.lower())
    if system == 'Windows':
        win_release, win_version, win_csd, win_ptype = platform.win32_ver()
        print('\twin_release =', win_release)
        print('\twin_version =', win_version)
        print('\twin_csd =', win_csd)
        print('\twin_ptype =', win_ptype)
        guesses.append(system.lower())
        guesses.append('garfield')

    guesses = list(set(guesses))  # remove duplicates
    return guesses


def chooseCfg():
    """
    Let the user choose a configuration files among the possible ones.
    """
    # try to find the guesses among the available files in oo_meta/CMake
    guesses = guessSystem()
    print('current working dir=', os.getcwd())
    avfiles = os.listdir(os.path.join('oo_meta', 'CMake'))
    cfiles = []
    for g in guesses:
        for avf in avfiles:
            if avf.find(g) != -1:
                cfiles.append(avf)

    # ask the user to make a choice
    print('Choose a config file:')
    for i, cf in enumerate(cfiles):
        print('\t%d: %s' % (i+1, cf))
    ii = input('? ')
    return cfiles[int(ii)-1]


def rmFolder(foldername):
    """remove a folder from disk.
    """
    if os.path.isdir(foldername):
        print('removing build dir')
        # http://stackoverflow.com/questions/16373747/permission-denied-doing-os-mkdird-after-running-shutil-rmtreed-in-python
        # avoid the failure of os.mkdir() is same name is used
        os.rename(foldername, f'{foldername}_trash')
        shutil.rmtree(f'{foldername}_trash')


def buildProject(projectname):
    """parallel build of a cmake project using incredibuild 
    if present, or makefiles.
    """
    if pu.isInstalled("BuildConsole") and os.path.isfile(f'{projectname}.sln'):
        print("[using incredibuild]")
        cmd = ['BuildConsole', f'{projectname}.sln',
               '/rebuild', '/cfg=Release|x64']
        subprocess.call(cmd)
    else:
        ncores = multiprocessing.cpu_count()
        print("[using cmake --build] with %d core(s)" % ncores)
        cmd = ['cmake', '--build', '.', '--config', 'Release']
        if os.path.isfile('Makefile'):
            cmd.extend(['--', '-j%d' % ncores])
        subprocess.call(cmd)


def main(repos, opts):

    # checkout/update everything
    build_mtf_required = False
    build_keygen_required = False
    print('checking remote repos:')
    for rep in repos:
        outdated = rep.outdated()
        # outdated = False
        print('\t', rep.name, ": outdated =", outdated)
        if outdated:
            build_mtf_required = True
            build_keygen_required = True

    # check presence of Metafor binaries
    if not os.path.isdir('oo_metaB'):
        print('oo_metaB folder is missing!')
        build_mtf_required = True
    else:
        possiblebins = [os.path.join('oo_metaB', 'bin', 'Metafor'),
                        os.path.join('oo_metaB', 'bin', 'Release', 'Metafor.exe')]
        if not (True in map(os.path.isfile, possiblebins)):
            print('Metafor executable not found!')
            build_mtf_required = True

    # check presence of keygen binaries
    if not os.path.isdir(os.path.join('keygen', 'build')):
        print('keygen/build folder is missing!')
        build_keygen_required = True
    else:
        possiblebins = [os.path.join('keygen', 'build', 'bin', 'keygen'),
                        os.path.join('keygen', 'build', 'bin', 'Release', 'keygen.exe')]
        if not (True in map(os.path.isfile, possiblebins)):
            print('keygen executable not found!')
            build_keygen_required = True

    # ask if user wants to force the builds
    if not build_mtf_required:
        print('=> Metafor build seems NOT to be required')
        print('do you want to force the build (y/[n])?')
        c = pu.getch()
        if c == 'y' or c == 'Y':
            build_mtf_required = True
    else:
        print('=> Metafor build is required')

    if not build_keygen_required:
        print('=> keygen build seems NOT to be required')
        print('do you want to force the build (y/[n])?')
        c = pu.getch()
        if c == 'y' or c == 'Y':
            build_keygen_required = True
    else:
        print('=> keygen build is required')

    # update all the folders regardless of dependencies
    if build_mtf_required or build_keygen_required:
        for rep in repos:
            rep.update()

    # build Metafor if needed
    if build_mtf_required:

        cfg = chooseCfg()  # requires 'oo_meta/CMake' to be checked out!

        # create a new Metafor binary folder
        rmFolder('oo_metaB')  # clean build dir
        os.mkdir('oo_metaB')  # could fail ('access denied') on Windows:
        pu.chDir('oo_metaB')

        # run cmake on Metafor
        cmd = ['cmake', '-C', os.path.join('..', 'oo_meta', 'CMake', cfg),
               os.path.join('..', 'oo_meta')]
        subprocess.call(cmd)

        # build Metafor using incredibuild if present
        buildProject('Metafor')
        pu.chDir('..')

    # build keygen if needed
    if build_keygen_required:
        if os.path.isdir('keygen'):
            pu.chDir('keygen')
            # create a new Metafor binary folder
            rmFolder('build')  # clean build dir
            os.mkdir('build')  # could fail ('access denied') on Windows:
            pu.chDir('build')

            # run cmake on keygen
            if pu.isUnix():
                cmd = ['cmake', '..']
            else:
                cmd = ['cmake', '-A', 'x64', '..']
            subprocess.call(cmd)

            # build Keygen using incredibuild if present
            buildProject('Keygen')
            pu.chDir('..')
            pu.chDir('..')


if __name__ == "__main__":
    
    # options (not finished yet!)
    opts = {
        'build_type': {
            'type': 'combo',
            'value': 'full',
            'values': ['full', 'student']
        },
    }

    # fills the repository list
    repos = []
    repos.append(vrs.GITRepo(
        'MetaforSetup', 'git@gitlab.uliege.be:am-dept/MN2L/MetaforSetup.git'))
    repos.append(vrs.GITRepo(
        'linuxbin', 'git@gitlab.uliege.be:am-dept/linuxbin.git'))
    repos.append(vrs.GITRepo(
        'oo_meta', 'git@gitlab.uliege.be:am-dept/MN2L/oo_meta.git'))

    if opts['build_type']['value'] == 'full':
        repos.append(vrs.GITRepo(
            'oo_nda', 'git@gitlab.uliege.be:am-dept/MN2L/oo_nda.git'))
        repos.append(vrs.GITRepo(
            'parasolid', 'git@gitlab.uliege.be:am-dept/MN2L/parasolid.git'))
        repos.append(vrs.GITRepo(
            'keygen', 'git@gitlab.uliege.be:am-dept/keygen.git'))

    main(repos, opts)
