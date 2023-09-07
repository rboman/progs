#!/usr/bin/env python3
# -*- coding: utf8 -*-

# py f:\dev\progs\bin\clean_fortran.py ricks.f90

# f:\f90ppr\moware\f90ppr.exe < tmp.f90 > out.txt

import sys, os, subprocess
sys.path.append(r'C:\msys64\mingw64\bin')

f90ppr_exe = r"F:\f90ppr\moware\f90ppr"

def main(fname):

    # tmpname = 'tmp.f90'
    if not os.path.isfile(fname):
        raise Exception(f'{fname} not found!')

    # with open(tmpname,'w') as tmpf:
    #     tmpf.write('$define FPPR_MAX_LINE 100\n')
    #     tmpf.write('$define FPPR_KWD_CASE FPPR_LOWER\n')
    #     tmpf.write('$define FPPR_USR_CASE 0\n')
    #     tmpf.write('$define FPPR_STP_INDENT 4\n')
    #     tmpf.write('$define FPPR_FXD_IN 0\n')
    #     tmpf.write('$define FPPR_FXD_OUT 0\n') 
    #     with open(f,'r') as infile:
    #         for l in infile.readlines():
    #             tmpf.write(l)

    base, ext = os.path.splitext(fname)
    outname = base+'.ppr'+ext

    outfile = open(outname,'wb')
    cmd = [ f90ppr_exe ]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=outfile)
    # maximum  line length (2-132)
    p.stdin.write(b'$define FPPR_MAX_LINE 132\n')
    # keywords case: FPPR_LEAVE, FPPR_UPPER, FPPR_LOWER
    p.stdin.write(b'$define FPPR_KWD_CASE FPPR_UPPER\n')
    # variables case: FPPR_LEAVE, FPPR_UPPER, FPPR_LOWER
    p.stdin.write(b'$define FPPR_USR_CASE FPPR_UPPER\n')
    # indentation (0-60)
    p.stdin.write(b'$define FPPR_STP_INDENT 4\n')
    # input format:  0=free format
    p.stdin.write(b'$define FPPR_FXD_IN 0\n')
    # output format:  0=free format
    p.stdin.write(b'$define FPPR_FXD_OUT 0\n')
    with open(fname,'rb') as infile:
        for l in infile.readlines():
            p.stdin.write(l)
    p.stdin.close()
    retcode = p.wait()
    print(f'retcode={retcode}')
    outfile.close()


    # if os.path.isfile(tmpname):
    #     os.remove(tmpname)


if __name__=="__main__":

    f = sys.argv[1]
    main(f)
