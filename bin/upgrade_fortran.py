#!/usr/bin/env python3
# -*- coding: utf8 -*-

# this script:
#  - upgrades .f files to .f90 free-format
#  - splits them into separate routines

# It has been used to upgrade/split the LAM3 fortran source files

# preliminary checks:
# - replace variables named "type" in f77 code (this is a f90 keyword)
# - replace "dowhile" by "do while" (fixed format does not care about spaces) 
# translation:
# - go to the source folder
# - run upgrade_fortran.py *.f
# - powergrep include extensions ".inc" => ".inc90"

import sys
import os
import subprocess
import shutil
import glob

# setup system to be able to call f90ppr
sys.path.append(r'C:\msys64\mingw64\bin')
f90ppr_exe = r"F:\f90ppr\moware\f90ppr"
f90split_exe = r"F:\f90ppr\moware\f90split"


def main(f77name):

    # convert file to absolute path
    f77name = os.path.abspath(f77name)

    # checks that the file exists
    if not os.path.isfile(f77name):
        raise Exception(f'{f77name} not found!')

    # CONVERT .f fixed-format file to .f90 free-format -------------------------

    # only processes .f
    base, ext = os.path.splitext(f77name)
    if ext != '.f':
        print(f'ignoring {f77name}')
        return
    f90name = base+'.f90'

    # processes the file with "f90ppr"
    print(f'f90ppr {f77name} => {f90name}')
    with open(f90name, 'wb') as f90file:
        cmd = [f90ppr_exe]
        p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=f90file)
        # maximum  line length (2-132)
        p.stdin.write(b'$define FPPR_MAX_LINE 120\n') # 132 produces errors (missing '&' at the EOL!)
        # keywords case: FPPR_LEAVE, FPPR_UPPER, FPPR_LOWER
        p.stdin.write(b'$define FPPR_KWD_CASE FPPR_LOWER\n')
        # variables case: FPPR_LEAVE, FPPR_UPPER, FPPR_LOWER
        p.stdin.write(b'$define FPPR_USR_CASE FPPR_LEAVE\n')
        # indentation (0-60)
        p.stdin.write(b'$define FPPR_STP_INDENT 4\n')
        # input format:  0=free format
        p.stdin.write(b'$define FPPR_FXD_IN 1\n')
        # output format:  0=free format
        p.stdin.write(b'$define FPPR_FXD_OUT 0\n')
        with open(f77name, 'rb') as infile:
            for l in infile.readlines():
                p.stdin.write(l)
        p.stdin.close()
        retcode = p.wait()
        if(retcode != 0):
            print(f'f90ppr ERROR: retcode = {retcode}')

    # remove the .f file
    print(f'rm {f77name}')
    os.remove(f77name)

    # -- SPLIT THE FILE INTO SUBROUTINES ---------------------------------------

    # runs f90split from another folder
    # otherwise, routines will have a bad name if the initial file already has
    # the name of a subroutine
    origdir = os.getcwd()
    tmpdir = 'split'
    if not os.path.isdir(tmpdir):
        os.mkdir(tmpdir)
    os.chdir(tmpdir)

    # checks that no .f90 file are in the tmp folder
    for f in os.listdir('.'):
        base, ext = os.path.splitext(f)
        if(ext == '.f90' or ext == '.mk'):
            print(f'rm {f}')
            os.remove(f)

    # calls f90split
    cmd = [f90split_exe]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE)
    with open(f90name, 'rb') as infile:
        for l in infile.readlines():
            p.stdin.write(l)
    p.stdin.close()
    retcode = p.wait()
    if(retcode != 0):
        print(f'f90split ERROR: retcode = {retcode}')

    # check that we do not have any "main000" files
    for f in os.listdir('.'):
        base, ext = os.path.splitext(f)
        if('main00' in os.path.basename(base)):
            print('ERROR!')
            raise Exception('something went wrong!')

    # remove the .f90 file
    print(f'rm {f90name}')
    os.remove(f90name)

    # copy subroutines to initial folder
    f90folder = os.path.dirname(f90name)

    for f in os.listdir('.'):
        base, ext = os.path.splitext(f)
        if(ext == '.f90'):
            print(f'mv {f} to {f90folder}')
            shutil.move(f, f90folder)
        elif(ext == '.mk'):
            print(f'rm {f}')
            os.remove(f)

    # go back to origdir
    os.chdir(origdir)


if __name__ == "__main__":
    
    # main(sys.argv[1])
    for f in glob.glob(sys.argv[1]):
        main(f)
