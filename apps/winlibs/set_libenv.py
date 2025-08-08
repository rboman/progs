#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# ajoute les variables d'environnement liées à mes libs (sauf PATH!)
#
# usage: set_libenv.py
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
    'BOOST_INCLUDEDIR': [r'%MYLOCAL%\boost'],  # not BOOST_INCLUDE_DIR!
    'BOOST_LIBRARYDIR': [r'%MYLOCAL%\boost\lib64-msvc-14.2'],
    #'CMAKE_INSTALL_PREFIX': [r'%MYLOCAL%\CGAL'],  # ??
    # if INCLUDE/LIB starts with %MYLOCAL% instead of c:\local, the variable editor
    # thinks it is not a list which makes it difficult to be edited (1 long line)... :(
    'INCLUDE':
    [
        rf"{local_folder}\Qwt\include",
        rf"{local_folder}\parasolid",
        rf"{local_folder}\lagamine\include",
        rf"{local_folder}\eigen",
        rf"{local_folder}\MUMPS\include",
        rf"{local_folder}\zlib\include",
        r"%oneapi_root%\mkl\latest\include",
        r"%oneapi_root%\tbb\latest\include",
        r"%oneapi_root%\compiler\latest\windows\compiler\include"
    ],
    'LIB':
    [
        rf"{local_folder}\Qwt\lib",
        rf"{local_folder}\lagamine\lib",
        rf"{local_folder}\MUMPS\lib",
        rf"{local_folder}\zlib\lib",
        # rf"{local_folder}\mesa3D",  # if VM
        r"%oneapi_root%\mkl\latest\lib\intel64",
        r"%oneapi_root%\tbb\latest\lib\intel64\vc14",
        r"%oneapi_root%\compiler\latest\windows\compiler\lib\intel64_win"
    ],
    'MYLOCAL':
    [
        rf"{local_folder}"
    ],
    'OMP_NUM_THREADS': [ "1" ],
    'MESA3D_ROOT': [ rf"{local_folder}\mesa3d" ],
    'P_SCHEMA': [rf"{local_folder}\parasolid\schema"],
    # 'Python3_ROOT_DIR': [ r"c:\Python37" ],  # <=
    # 'LMS_LICENSE': [ "xxx" ],
    # 'GIT_SSH': [ r"xxx" ],
}
# print(envs)
# import sys
# sys.exit()

if __name__ == '__main__':

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
