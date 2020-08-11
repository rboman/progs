#!/usr/bin/env python3
# -*- coding: utf8 -*-

# py f:\dev\progs\bin\clean_fortran.py ricks.f90

# f:\f90ppr\moware\f90ppr.exe < tmp.f90 > out.txt

import sys, os, subprocess, shutil
sys.path.append(r'C:\msys64\mingw64\bin')

f90ppr_exe = r"F:\f90ppr\moware\f90ppr"

def main(fname):

    # tmpname = 'tmp.f90'
    if not os.path.isfile(fname):
        raise Exception(f'{fname} not found!')

    base, ext = os.path.splitext(fname)

    outname = base+'.ppr'+ext

    outfile = open(outname,'wb')
    cmd = [ f90ppr_exe ]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=outfile)
    # maximum  line length (2-132)
    p.stdin.write(b'$define FPPR_MAX_LINE 132\n')
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
    with open(fname,'rb') as infile:
        for l in infile.readlines():
            p.stdin.write(l)
    p.stdin.close()
    retcode = p.wait()
    print(f'retcode={retcode}')
    outfile.close()

    # overwrite file
    shutil.copy(outname, fname)
    # remove temporary
    if os.path.isfile(outname):
        os.remove(outname)


if __name__=="__main__":

    f = sys.argv[1]
    main(f)
