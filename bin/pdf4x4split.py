#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# This script takes a presentation pdf printed with 4 pages/sheet
# and converts it in a pdf with 1 page/sheet
#
# TODO: remove tmp folder (work in /tmp?)
#       clean output
#       handle errors (e.g. pdfjam, gs, ... not present in the system)

import sys, os, subprocess, re

def pdfsize(fname):
    try:
        cmd = ['pdfinfo', fname ]
        out = subprocess.check_output(cmd)
    except OSError:
        return 'pdfinfo not found'
    out = out.decode()  # python 3 returns bytes
    m = re.search(r'Page size:\s+(\d+) x (\d+)', out)
    if m and len(m.groups()) > 0:
        sx, sy = int(m.group(1)), int(m.group(2))
    else:
        raise Exception('cannot read "pdfinfo" output')
    m = re.search(r'Pages:\s+(\d+)', out)
    if m and len(m.groups()) > 0:
        np = int(m.group(1))
    else:
        raise Exception('cannot read "pdfinfo" output')
    return sx, sy, np


def splitpdf(fname):
    fnamef = os.path.abspath(fname)
    print 'processing', fnamef

    # create a tmp folder
    tmpdir = os.path.join(os.path.dirname(fnamef), 'tmp')
    print 'tmpdir=', tmpdir
    if not os.path.isdir(tmpdir):
        os.mkdir(tmpdir)
    
    # separate pdf (1 pdf per page)
    pattern = os.path.join(tmpdir, r'Page%04d.pdf')
    cmd = ['pdfseparate', fname, pattern]
    print cmd
    subprocess.call(cmd)

    # get the size of the pdf
    sx, sy, np = pdfsize(fname)
    print 'pdf size=%dx%d' %(sx, sy)
    print 'pdf has %d pages' % np

    # crop pdfs

    files = []
    for i in range(1,np+1):
        infile = pattern % i
        dx = int(sx/2)
        dy = int(sy/2)
        ox=0
        oy=0
        k = 0
        for i in reversed(range(2)):
            for j in range(2):
                oy = i*dy
                ox = j*dx
                outfile = infile.replace('.pdf', '_%d.pdf' % k)
                files.append(outfile)
                cmd = ['pdfcrop', '--bbox', 
                    '%d %d %d %d' % (ox, oy, ox+dx, oy+dy),  
                    infile, outfile]
                print cmd
                subprocess.call(cmd)
                k+=1
        # del pages
        os.unlink(infile)
    
    currentdir = os.getcwd()

    os.chdir(tmpdir)    # -- goto tmp folder
    cmd=['pdfjoin']+files
    print cmd
    subprocess.call(cmd)

    joinedfile = files[-1].replace('.pdf','-joined.pdf')
    outputfile = joinedfile.replace('.pdf','-trim-a6.pdf')
    # printe le resultat avec ghostview (pour virer tout ce qui n'est pas visible)
    cmd=['gs', '-dNOPAUSE', '-dBATCH', '-sDEVICE=pdfwrite',
        '-dCompatibilityLevel=1.4', '-dPDFSETTINGS="/ebook"', 
        '-sOutputFile=%s' % outputfile, 
        joinedfile]
    print cmd
    subprocess.call(cmd)

    os.chdir(currentdir) # -- back to initial folder
   
    inputfile = os.path.join(tmpdir, outputfile)
    outputfile = fname.replace('.pdf','-1x1.pdf')
    cmd= ['pdfjam', '--outfile', outputfile, '--paper', 
        'a4paper', '--landscape', inputfile]
    print cmd
    subprocess.call(cmd)


if __name__=="__main__":
    fname = sys.argv[1]
    splitpdf(fname)
