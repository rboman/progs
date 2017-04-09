#! /usr/bin/env python
# -*- coding: latin-1; -*-

import os, shutil, platform, subprocess
import pytools.utils as pu
import pytools.versioning as vrs

def main(repos):

    for rep in repos:
        rep.update()

    # clean build dir
    if 1:
        print 'removing build dir'
        if os.path.isdir('oo_metaB'):
            os.rename('oo_metaB','oo_metaB_trash') # avoid the failure of os.mkdir() is same name is used
            shutil.rmtree('oo_metaB_trash')
        os.mkdir('oo_metaB') # could fail (access denied) on Windows: 
        # http://stackoverflow.com/questions/16373747/permission-denied-doing-os-mkdird-after-running-shutil-rmtreed-in-python
        pu.chDir('oo_metaB')
    
    # make
    if 1:
        if pu.isUnix():
            cmd='cmake -G"Eclipse CDT4 - Unix Makefiles"' \
                ' -D_ECLIPSE_VERSION=4.7' \
                ' -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE' \
                ' -C ../oo_meta/CMake/ubuntu.cmake' \
                ' ../oo_meta'
        else:
            cmd=r'cmake -C ..\oo_meta\CMake\garfield.cmake ..\oo_meta'
        os.system(cmd)
        
    if 1:
        if pu.isUnix():
            os.system('make -j 12')
        else:
            os.system('BuildConsole Metafor.sln /rebuild /cfg="Release|x64"')

if __name__=="__main__":
    repos = []
    repos.append(vrs.GITRepo('keygen', 'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/keygen.git'))
    repos.append(vrs.GITRepo('MetaforSetup', 'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/MetaforSetup.git'))
    repos.append(vrs.GITRepo('parasolid', 'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/parasolid.git'))
    repos.append(vrs.GITRepo('linuxbin', 'git@github.com:ulgltas/linuxbin.git'))        
    repos.append(vrs.SVNRepo('oo_meta', 'svn+ssh://boman@blueberry.ltas.ulg.ac.be/home/metafor/SVN/oo_meta/trunk'))
    repos.append(vrs.SVNRepo('oo_nda', 'svn+ssh://boman@blueberry.ltas.ulg.ac.be/home/metafor/SVN/oo_nda/trunk'))

    main(repos)
    
