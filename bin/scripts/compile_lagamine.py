#!/usr/bin/env python

import os, subprocess
import pytools.versioning as vrs
import pytools.utils as pyu

def main():

    repo = vrs.GITRepo('Lagamine', 'git@gitlab.uliege.be:UEE/Lagamine.git')
    repo.update()
    repo.checkout('romain')

    repo = vrs.GITRepo('LagamineAPI', 'git@gitlab.uliege.be:am-dept/MN2L/LagamineAPI.git')
    repo.update()
    repo.checkout('romain')

    os.chdir('LagamineAPI')

    # remove folder 'build'
    if os.path.isdir('build'): 
        print('removing build dir')
        # http://stackoverflow.com/questions/16373747/permission-denied-doing-os-mkdird-after-running-shutil-rmtreed-in-python   
        os.rename('build','build_trash') # avoid the failure of os.mkdir() is same name is used
        shutil.rmtree('build_trash')

    # create folder 'build'
    os.mkdir('build') # could fail (access denied) on Windows:
    pu.chDir('build')

    # cmake

    #cmd = ['cmake', '-C', os.path.join('..','oo_meta','CMake',cfg), os.path.join('..','oo_meta') ]
    #subprocess.call(cmd)    
    # TODO...



if __name__=='__main__':
    
    main()