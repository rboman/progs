#!/usr/bin/env python3
# -*- coding: utf8 -*-

# this script:
#  - upgrades .f files to .f90 free-format 
#  - splits them into separate routines


import sys, os, subprocess
sys.path.append(r'C:\msys64\mingw64\bin')

f90ppr_exe = r"F:\f90ppr\moware\f90ppr"
f90split_exe = r"F:\f90ppr\moware\f90split"

def main(fname):

    fname = os.path.abspath(fname)

    # checks that the file exists
    if not os.path.isfile(fname):
        raise Exception(f'{fname} not found!')

    # CONVERT .f fixed-format file to .f90 free-format -------------------------

    # only processes .f
    base, ext = os.path.splitext(fname)
    if ext!='.f':
        print(f'ignoring {fname}')
        return
    outname = base+'.f90'

    # processes the file with "f90ppr"
    print(f'f90ppr {fname} => {outname}')
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

    # -- SPLIT THE FILE INTO SUBROUTINES ---------------------------------------

    tmpdir = 'split'
    if not os.path.isdir(tmpdir):
        os.mkdir(tmpdir)
    os.chdir(tmpdir)

    cmd = [ f90split_exe ]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE)
    with open(outname,'rb') as infile:
        for l in infile.readlines():
            p.stdin.write(l)
    p.stdin.close()
    retcode = p.wait()
    print(f'retcode={retcode}')
   


    # if os.path.isfile(tmpname):
    #     os.remove(tmpname)


if __name__=="__main__":

    main(sys.argv[1])
    # for f in glob.glob(sys.argv[1]):
    #     main(f)

