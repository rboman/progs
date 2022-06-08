#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# ajoute les variables d'environnement liées à mes libs
#
# variables utilisateur système vierge:
#   OneDrive
#   OneDriveConsumer
#   Path
#   TEMP
#   TMP

import os, subprocess

local_folder = r"c:\local"

envs = {
    'BOOST_INCLUDEDIR': [ r'%MYLOCAL%\boost' ],  # not BOOST_INCLUDE_DIR!
    'BOOST_LIBRARYDIR': [ r'%MYLOCAL%\boost\lib64-msvc-14.2' ],
    'CMAKE_INSTALL_PREFIX': [ r'%MYLOCAL%\CGAL' ],
    'INCLUDE':
    [
        r"%MYLOCAL%\Qwt\include",
        r"%MYLOCAL%\parasolid",
        r"%MYLOCAL%\lagamine\include",
        r"%MYLOCAL%\eigen",
        r"%MYLOCAL%\MUMPS\include",
        r"%MYLOCAL%\zlib\include",
        r"%ICPP_COMPILER19%\mkl\include",
        r"%ICPP_COMPILER19%\tbb\include",
        r"%ICPP_COMPILER19%\compiler\include",        
    ],
    'LIB':
    [
        r"%MYLOCAL%\Qwt\lib",
        r"%MYLOCAL%\lagamine\lib",
        r"%MYLOCAL%\MUMPS\lib",
        r"%MYLOCAL%\zlib\lib",
        r"%ICPP_COMPILER19%\mkl\lib\intel64",
        r"%ICPP_COMPILER19%\tbb\lib\intel64\vc14",
        r"%ICPP_COMPILER19%\compiler\lib\intel64"
    ],
    'MYLOCAL':
    [
        rf"{local_folder}"
    ],
    # 'OMP_NUM_THREADS': [ "1" ],  
    'P_SCHEMA': [ rf"{local_folder}\parasolid\schema" ],  
    # 'Python3_ROOT_DIR': [ r"c:\Python37" ],  # <=     
    # 'LMS_LICENSE': [ "xxx" ],     
    # 'GIT_SSH': [ r"xxx" ],     
}
# print(envs)
# import sys
# sys.exit()

if __name__=='__main__':

    for key, newpaths in envs.items():
        #print(f'{key} : {folders}')
        # clean new paths to be added (c:/loCal => c:\\local)
        newpaths = list(map(os.path.normcase, map(os.path.normpath, newpaths)))
        # retrieve env key & convert to list
        try:
            oldpaths = os.environ[key]
            oldpaths = oldpaths.strip().strip(';').split(';')
            oldpaths = list(map(os.path.normcase, map(os.path.normpath, oldpaths)))
        except:
            oldpaths = [] 
        # print(f'{key} = {oldpaths}')
        
        for f in newpaths:
            # check new folders on disk
            # if not os.path.isdir(f):
            #     raise Exception("{f} not found on disk.")
            if not f in oldpaths:
                oldpaths.append(f)

        # set values
        newvalue = ';'.join(oldpaths)
        cmd = ['setx', key, newvalue]
        print(' '.join(cmd))
        subprocess.call(cmd)
