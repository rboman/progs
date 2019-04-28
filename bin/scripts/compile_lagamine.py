#!/usr/bin/env python
#
# This script builds the lagamine API for Metafor (branch 'romain')
# usage:
#   rb.py compile_lagamine.py
#

import os
import subprocess
import shutil
import pytools.versioning as vrs
import pytools.utils as pyu


def main():

    # checkout/update 'Lagamine' source code and switch to branch 'romain'
    repo = vrs.GITRepo('Lagamine', 'git@gitlab.uliege.be:UEE/Lagamine.git')
    repo.update()
    repo.checkout('romain')

    # checkout/update 'LagamineAPI' source code and switch to branch 'romain'
    repo = vrs.GITRepo(
        'LagamineAPI', 'git@gitlab.uliege.be:am-dept/MN2L/LagamineAPI.git')
    repo.update()
    repo.checkout('romain')

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
    pyu.chDir('build')

    # cmake [config/build/install] [Release/Debug]
    cmd = [
        'cmake',
        '-DLAGAMINE_USE_METIS=OFF',
        '-DLAGAMINE_USE_MKL=OFF',
        '-DCMAKE_INSTALL_PREFIX=f:/local/lagamine',
        '-A', 'x64',
        '..'
    ]
    subprocess.call(cmd)
    cmd = ['cmake', '--build', '.',
           '--config', 'Release', '--target', 'INSTALL']
    subprocess.call(cmd)
    cmd = ['cmake', '--build', '.',
           '--config', 'Debug', '--target', 'INSTALL']
    subprocess.call(cmd)


if __name__ == '__main__':
    main()
