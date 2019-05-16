#!/usr/bin/env python
#
# This script builds the lagamine API for Metafor (branch 'romain')
# usage:
#   rb.py compile_lagamine.py
#
# tested on Win10 only

import os
import subprocess
import shutil
import pytools.versioning as vrs
import pytools.utils as pu
import datetime


def main():

    o = {
        'target_folder': '/opt' if pu.isUnix() else 'f:/local',
        'target_name': 'lagamine',
        'branch': 'romain'
    }
    print('options =', o)

    # make a backup in current folder
    if os.path.isdir(os.path.join(o['target_folder'], o['target_name'])):
        now = datetime.datetime.now()
        arcname = shutil.make_archive('%s-%s' % (o['target_name'], now.strftime('%Y-%m-%d-%I%M%S')),
                                      format='zip', 
                                      root_dir=o['target_folder'], 
                                      base_dir=o['target_name'],)
        print ('%s created.' % arcname)

    # checkout/update 'Lagamine' source code and switch to branch 'romain'
    repo = vrs.GITRepo('Lagamine', 'git@gitlab.uliege.be:UEE/Lagamine.git')
    repo.update()
    repo.checkout(o['branch'])

    # checkout/update 'LagamineAPI' source code and switch to branch 'romain'
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
        '-DLAGAMINE_USE_METIS=OFF',
        '-DLAGAMINE_USE_MKL=OFF',
        '-DCMAKE_INSTALL_PREFIX=%s/%s' % (
            o['target_folder'], o['target_name'])
    ]
    if not pu.isUnix():
        cmd.extend(['-A', 'x64'])
    cmd.append('..')
    print (cmd)
    iop = subprocess.call(cmd)
    if(iop):
        raise Exception('\n\n\t** ERROR: cmake exited with status %d\n' % iop)
    print ('iop=', iop)

    # build/install
    if pu.isUnix():
        # Release only then sudo for install
        cmd = ['cmake', '--build', '.',
               '--config', 'Release']
        subprocess.call(cmd)
        cmd = ['sudo', 'make', 'install']
        print('\nINSTALLING... Enter your passwd for\n%s' % cmd)
        subprocess.call(cmd)
    else:
        # install both release & debug
        for cfg in ['Release', 'Debug']:
            cmd = ['cmake', '--build', '.',
                   '--config', cfg, '--target', 'INSTALL']
            subprocess.call(cmd)


if __name__ == '__main__':
    main()
