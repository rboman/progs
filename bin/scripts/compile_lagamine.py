#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This script builds the lagamine API for Metafor (branch 'papeleux')
# usage:
#   rb.py compile_lagamine.py
#
# works on Win10/linux/msys2-mingw64

import os
import subprocess
import shutil
import pytools.versioning as vrs
import pytools.utils as pu
import datetime


def main():

    o = {
        'target_folder': '/opt' if pu.isUnix() else 'c:/local',
        'target_name': 'lagamine',
        'branch': 'papeleux'
    }
    if pu.sysName() == 'mingw':
        o['target_folder'] = 'c:/msys64/opt'

    print('options =', o)

    # make a backup in current target binary folder (it will be overwritten)
    if os.path.isdir(os.path.join(o['target_folder'], o['target_name'])):
        now = datetime.datetime.now()
        arcname = shutil.make_archive('%s-%s' % (o['target_name'], now.strftime('%Y-%m-%d-%I%M%S')),
                                      format='zip', 
                                      root_dir=o['target_folder'], 
                                      base_dir=o['target_name'],)
        print ('%s created.' % arcname)

    # checkout/update 'Lagamine' source code and switch to selected branch
    repo = vrs.GITRepo('Lagamine', 'git@gitlab.uliege.be:UEE/Lagamine.git')
    repo.update()
    repo.checkout(o['branch'])

    # checkout/update 'LagamineAPI' source code and switch to selected branch
    repo = vrs.GITRepo(
        'LagamineAPI', 'git@gitlab.uliege.be:am-dept/MN2L/LagamineAPI.git')
    repo.update()
    repo.checkout(o['branch'])

    os.chdir('LagamineAPI')

    # remove folder 'build'
    if os.path.isdir('build'):
        print('removing build dir')
        # http://stackoverflow.com/questions/16373747/permission-denied-doing-os-mkdird-after-running-shutil-rmtreed-in-python
        if os.path.isdir('build_trash'):
            shutil.rmtree('build_trash')
        # avoid the failure of os.mkdir() is same name is used
        os.rename('build', 'build_trash')
        shutil.rmtree('build_trash')

    # create folder 'build'
    os.mkdir('build')  # could fail (access denied) on Windows:
    pu.chDir('build')

    # configure
    cmd = [
        'cmake',
        '-DLAGAMINE_USE_METIS=OFF',  # could be removed
        '-DLAGAMINE_USE_MKL=OFF',  # could be removed
        '-DCMAKE_INSTALL_PREFIX=%s/%s' % (
            o['target_folder'], o['target_name'])
    ]
    # if not pu.isUnix():
    #     cmd.extend(['-A', 'x64'])
    cmd.append('..')
    print (cmd)
    iop = subprocess.call(cmd)
    if(iop):
        raise Exception('\n\n\t** ERROR: cmake exited with status %d\n' % iop)
    print('iop=', iop)

    # build/install
    if pu.sysName() == 'linux':
        # Release only then sudo for install
        cmd = ['cmake', '--build', '.',
               '--config', 'Release', '--', '-j', '8']
        subprocess.call(cmd)
        cmd = ['sudo', 'make', 'install']
        print('\nINSTALLING... Enter your passwd for\n%s' % cmd)
        subprocess.call(cmd)
    elif pu.sysName() == 'mingw':
        cmd = [ 'ninja', 'install' ]
        subprocess.call(cmd)
    else:
        # install both release & debug
        for cfg in ['Release', 'Debug']:
            cmd = ['cmake', '--build', '.',
                   '--config', cfg, '--target', 'INSTALL']
            subprocess.call(cmd)


if __name__ == '__main__':
    main()
