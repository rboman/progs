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

envs = {
    'BOOST_INCLUDEDIR': [ r'c:\local\boost' ],  # not BOOST_INCLUDE_DIR!
    'BOOST_LIBRARYDIR': [ r'c:\local\boost\lib64-msvc-14.2' ],
    'CMAKE_INSTALL_PREFIX': [ r'c:\local\CGAL' ],
    'INCLUDE':
    [
        r"c:\local\Qwt\include",
        r"c:\local\parasolid",
        r"c:\local\lagamine\include",
        r"c:\local\eigen"
    ],
    'LIB':
    [
        r"c:\local\Qwt\lib",
        r"c:\local\lagamine\lib"
    ],
    'MYLOCAL':
    [
        r"c:\local"
    ],
    # 'OMP_NUM_THREADS': [ "1" ],  
    'P_SCHEMA': [ r"c:\local\parasolid\schema" ],  
    # 'Python3_ROOT_DIR': [ r"c:\Python37" ],  # <=     
    # 'LMS_LICENSE': [ "xxx" ],     
    # 'GIT_SSH': [ r"xxx" ],     
}
# print(envs)

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
