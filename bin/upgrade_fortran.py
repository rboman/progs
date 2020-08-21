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
#
# new interface:
# upgrade_fortran check *.f                # optional 
# upgrade_fortran freeformat *.f           # runs findent (does not truncate comments!)
# upgrade_fortran pretty *.f               # runs f90ppr (nicer output)
# upgrade_fortran split *.f90              # runs f90split


import sys
import os
import subprocess
import shutil
import glob
import platform

if 'Windows' in platform.uname():
    # setup system to be able to call f90ppr
    sys.path.append(r'C:\msys64\mingw64\bin')
    f90ppr_exe = r"F:\f90ppr\moware\f90ppr"
    f90split_exe = r"F:\f90ppr\moware\f90split"
    findent_exe = r"F:\findent-3.1.6\findent"
else:
    f90ppr_exe = r"f90ppr"
    f90split_exe = r"f90split"
    findent_exe = r"findent"

def check_one(f77name):
    """ performs some preliminary checks in the source files
    """
    # checks line length (f90ppr may truncate long lines of comments)
    maxlen = 0
    maxno = 0
    warns = []
    with open(f77name, 'rb') as infile:
        for i,l in enumerate(infile.readlines()):
            # checks line length
            clen = len(l)
            if clen>132:
                warns.append(f'{os.path.basename(f77name)}:{i+1} line exceeds 132 columns!')
            if clen>maxlen:
                maxlen = clen
                maxno = i+1
            # checks some bad patterns
            if b'dowhile' in l.lower():
                warns.append(f'{os.path.basename(f77name)}:{i+1} replace "dowhile" by "do while"!\n\t'+l.decode().strip())
            # if b'type' in l.lower():
            # faire une regex plus subtile! (supprimer commentaires, chaines, variables "typeel")
            #     warns.append(f'{os.path.basename(f77name)}:{i+1} "type" is a reserved keyword in f90!\n\t'+l.decode().strip())

    #print(f'longest line ({maxlen} chars) at line {maxno}')
    for w in warns:
        print(w)


def pretty_one(f90name, keepf=False):
    """ process .f90 through f90ppr
    """

    base, ext = os.path.splitext(f90name)
    #f90name = base+'.f90'

    # rename orig file to .f90.bak
    bakfile = f90name+'.bak'
    if os.path.isfile(bakfile):
        os.remove(bakfile)
    os.rename(f90name, bakfile)

    # processes the file with "f90ppr"
    print(f'f90ppr {os.path.basename(f90name)}')
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
        p.stdin.write(b'$define FPPR_FXD_IN 0\n')
        # output format:  0=free format
        p.stdin.write(b'$define FPPR_FXD_OUT 0\n')
        with open(bakfile, 'rb') as infile:
            for l in infile.readlines():
                p.stdin.write(l)
        p.stdin.close()
        retcode = p.wait()
        if(retcode != 0):
            print(f'f90ppr ERROR: retcode = {retcode}')

    # remove/rename the .f90.bak file
    if not keepf:
        print(f'rm {bakfile}')
        os.remove(bakfile)


def freeformat_one(f77name, keepf=False):
    """ CONVERT .f fixed-format file to .f90 free-format with findent
    """

    base, ext = os.path.splitext(f77name)
    opts = []
    if ext=='.inc':
        f90name = base+'.inc90'
    else:
        f90name = base+'.f90'

    # do checks
    check_one(f77name)

    # processes the file with "f90ppr"
    print(f'findent {os.path.basename(f77name)} => {os.path.basename(f90name)}')
    with open(f90name, 'wb') as f90file:
        with open(f77name, 'rb') as infile:
            cmd = [findent_exe, '-i4', '-ofree']
            p = subprocess.Popen(cmd, stdin=infile, stdout=f90file)
            retcode = p.wait()
            if(retcode != 0):
                print(f'findent ERROR: retcode = {retcode}')

    # remove/rename the .f file
    if not keepf:
        print(f'rm {f77name}')
        os.remove(f77name)
    else:
        bak = f77name+'.bak'
        os.rename(f77name, bak)




def freeformat_f90ppr_one(f77name, keepf=False):
    """ CONVERT .f fixed-format file to .f90 free-format with f90ppr
    this routine do not read comments after line 72 in free format
    if these comments are placed after a command
    (comments spanning a whole line are OK)
    """

    base, ext = os.path.splitext(f77name)
    f90name = base+'.f90'

    # do checks
    check_one(f77name)

    # processes the file with "f90ppr"
    print(f'f90ppr {os.path.basename(f77name)} => {os.path.basename(f90name)}')
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

    # remove/rename the .f file
    if not keepf:
        print(f'rm {f77name}')
        os.remove(f77name)
    else:
        bak = f77name+'.bak'
        os.rename(f77name, bak)


def split_one(f90name, keepf=False):
    """ SPLIT .f90 files, 1 file per subroutine
    """
 
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
        raise Exception(f'f90split ERROR: retcode = {retcode}')

    # check that we do not have any "main000" files
    for f in os.listdir('.'):
        base, ext = os.path.splitext(f)
        if('main00' in os.path.basename(base)):
            print('ERROR!')
            raise Exception('something went wrong!')

    # remove the .f90 file
    if not keepf:
        print(f'rm {f90name}')
        os.remove(f90name)
    else:
        bak = f90name+'.bak'
        os.rename(f90name, bak)  

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


def iterate(files, exts=['.f', '.for', '.f90','.inc']):
    """ iterates over the files with given extension and 
    performs some checks. 
    Yields valid absolute filenames one by one.
    """
    for f in files:
        for gf in glob.glob(f):
            # convert file to absolute path
            gf = os.path.abspath(gf)
            # check extension
            base, ext = os.path.splitext(gf)
            if not ext in exts:
                print(f'ignoring {gf}')
                continue
            # check whether file exists
            if not os.path.isfile(gf):
                raise Exception(f'{gf} not found!')
            yield gf


def check(files):
    for f in iterate(files, exts=['.f', '.for','.inc']):
        print(f'checking file {f}')
        check_one(f)

def split(files, keep):
    for f in iterate(files, exts=['.f90']):
        print(f'splitting file {f}')
        split_one(f, keep)

def freeformat(files, keep):
    for f in iterate(files, exts=['.f', '.for', '.f90', '.inc']):
        print(f'converting file {f} to free format')
        freeformat_one(f, keep)

def pretty(files, keep):
    for f in iterate(files, exts=['.f90','.inc90']):
        print(f'f90ppr {f}')
        pretty_one(f, keep)


if __name__ == "__main__":


    # parse cmd-line arguments
    import argparse
    parser = argparse.ArgumentParser(description='Upgrade old FORTRAN.')
    parser.add_argument("--keep", help="keep original files", action="store_true")
    # parser.add_argument("--include", help="include pattern", default='')
    # parser.add_argument("--exclude", help="exclude pattern", default='')
    parser.add_argument('command', help='command', choices=[
                        'check', 'split', 'freeformat', 'pretty'])
    parser.add_argument('files', nargs='+', help='fortran files')
    args = parser.parse_args()
    print (args)

    if args.command == 'check':
        check(args.files)
    elif args.command == 'split':
        split(args.files, args.keep)
    elif args.command == 'freeformat':
        freeformat(args.files, args.keep)
    elif args.command == 'pretty':
        pretty(args.files, args.keep)
    else:
        raise Exception("Unknown arg: {}".format(args.command))


    # main(sys.argv[1])

