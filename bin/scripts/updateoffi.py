#! /usr/bin/env python
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

from __future__ import print_function
from builtins import input
import os, shutil, platform, subprocess, multiprocessing
import pytools.utils as pu
import pytools.versioning as vrs

def guessSystem():
    guesses = []
    import platform
    system, node, release, version, machine, processor = platform.uname()

    # machine name
    machine_name = node.split('.')[0].split('-')[0].lower()
    print('machine_name =', machine_name)
    guesses.append(machine_name)

    # robo's libs
    #print os.environ['MYLOCAL']
    if 'MYLOCAL' in os.environ:
        guesses.append('garfield')

    # system name
    print('system =', system)
    if system=='Darwin':
        mac_release, mac_versioninfo, mac_machine = platform.mac_ver()  
        print('\tmac_release =', mac_release)
        print('\tmac_versioninfo =', mac_versioninfo)
        print('\tmac_machine =', mac_machine)
        guesses.append('macos')
    if system=='Linux':
        lin_distname, lin_version, lin_id = platform.linux_distribution()
        print('\tlin_distname =', lin_distname)
        print('\tlin_version =', lin_version)
        print('\tlin_id =', lin_id)
        guesses.append(lin_distname.lower())
    if system=='Windows':
        win_release, win_version, win_csd, win_ptype = platform.win32_ver()
        print('\twin_release =', win_release)
        print('\twin_version =', win_version)
        print('\twin_csd =', win_csd)
        print('\twin_ptype =', win_ptype)
        guesses.append(system.lower())

    guesses = list(set(guesses))  # remove duplicates
    return guesses

def chooseCfg():
    guesses = guessSystem()
    #print guesses

    avfiles = os.listdir('oo_meta/CMake')
    #print avfiles
    cfiles=[]
    for g in guesses:
        for avf in avfiles:
            #print "testing", avf, g
            if avf.find(g)!=-1:
                cfiles.append(avf)
    #print cfiles

    # ask
    print('Choose a config file:')
    for i, cf in enumerate(cfiles):
        print('\t%d: %s' % (i+1, cf))
    ii = input('? ')
    return cfiles[int(ii)-1]


def main(repos, opts):

    # checkout/update everything
    build_required = False
    for rep in repos:
        outdated = rep.outdated()
        print(rep.name, ": outdated =", outdated)
        if outdated:
            build_required = True

    if not os.path.isdir('oo_metaB'):
        print('oo_metaB folder is missing!')
        build_required = True

    if not build_required:
        print('=> build is NOT required')
        print('do you want to force the build (y/[n])?')
        c = pu.getch()
        if c=='y' or c=='Y':
            build_required=True
    else:
        print('=> build is required')    

    if build_required:

        cfg = chooseCfg()

        # update
        for rep in repos:
            rep.update()
        
        # clean build dir
        
        if os.path.isdir('oo_metaB'): 
            print('removing build dir')
            # http://stackoverflow.com/questions/16373747/permission-denied-doing-os-mkdird-after-running-shutil-rmtreed-in-python   
            os.rename('oo_metaB','oo_metaB_trash') # avoid the failure of os.mkdir() is same name is used
            shutil.rmtree('oo_metaB_trash')

        # create folder

        os.mkdir('oo_metaB') # could fail (access denied) on Windows:
        pu.chDir('oo_metaB')

        # cmake

        if pu.isUnix():
            cmd='cmake -C ../oo_meta/CMake/%s ../oo_meta' %cfg
        else:
            cmd=r'cmake -C ..\oo_meta\CMake\%s ..\oo_meta' %cfg
        os.system(cmd)
    
        # build

        if pu.isInstalled("BuildConsole") and os.path.isfile('Metafor.sln'):
            print("[using incredibuild]")
            os.system('BuildConsole Metafor.sln /rebuild /cfg="Release|x64"')
        else:
            ncores = multiprocessing.cpu_count()
            print("[using cmake --build] with %d core(s)" % ncores)
            if pu.isUnix():
                os.system('cmake --build . --config Release -- -j%d' % ncores)
            else:
                os.system('cmake --build . --config Release')



if __name__ == "__main__":

    opts = {
        'build_type' : {
            'type': 'combo',
            'value': 'full',
            'values': ['full', 'student']
        },
    }

    repos = []
    repos.append(vrs.GITRepo('MetaforSetup', 'git@gitlab.uliege.be:am-dept/MN2L/MetaforSetup.git'))
    repos.append(vrs.GITRepo('linuxbin', 'git@github.com:ulgltas/linuxbin.git'))        
    repos.append(vrs.SVNRepo('oo_meta', 'svn+ssh://boman@blueberry.ltas.ulg.ac.be/home/metafor/SVN/oo_meta/trunk'))

    if opts['build_type']['value']=='full':
        repos.append(vrs.SVNRepo('oo_nda', 'svn+ssh://boman@blueberry.ltas.ulg.ac.be/home/metafor/SVN/oo_nda/trunk'))
        repos.append(vrs.GITRepo('parasolid', 'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/parasolid.git'))
        repos.append(vrs.GITRepo('keygen', 'git@gitlab.uliege.be:am-dept/MN2L/keygen.git'))

    main(repos, opts)
    