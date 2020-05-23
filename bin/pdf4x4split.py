#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2019 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#
# This script takes a presentation pdf printed with 4 pages/sheet
# and converts it in a pdf with 1 page/sheet
#
# TODO: handle errors (e.g. pdfjam, gs, ... not present in the system)
#
# REQUIRES: pdfinfo, pdfseparate, pdfjoin, gs, pdfjam
#

from past.utils import old_div
import sys, os, subprocess, re, shutil

def getpdfsize(fname):
    """ returns (size_x, size_y, number_of_pages)
    """
    try:
        cmd = ['pdfinfo', fname ]
        out = subprocess.check_output(cmd)
    except OSError:
        raise Exception('pdfinfo not found')
    out = out.decode()  # python 3 returns bytes
    m = re.search(r'Page size:\s+(\d+) x (\d+)', out)
    if m and len(m.groups())==2:
        sx, sy = int(m.group(1)), int(m.group(2))
    else:
        raise Exception('cannot read "pdfinfo" output')
    m = re.search(r'Pages:\s+(\d+)', out)
    if m and len(m.groups())==1:
        np = int(m.group(1))
    else:
        raise Exception('cannot read "pdfinfo" output')
    return sx, sy, np


def runcmd(cmd):
    with open(os.devnull, 'w') as FNULL:
        subprocess.call(cmd, stdout=FNULL, stderr=subprocess.STDOUT)


def splitpdf(fname, tmpdir):

    # separate pdf (1 pdf per page)
    print('\tseparating pages into 1-page files')
    pattern = os.path.join(tmpdir, r'Page%04d.pdf')
    cmd = ['pdfseparate', fname, pattern]
    runcmd(cmd)

    # get the size of the pdf
    sx, sy, np = getpdfsize(fname)
    #print 'pdf size = %d x %d' % (sx, sy)
    #print 'pdf has %d pages' % np

    # crop pdfs

    
    files = []
    for n in range(1,np+1):
        infile = pattern % n
        dx = int(old_div(sx,2))
        dy = int(old_div(sy,2))
        k = 0
        for i in reversed(list(range(2))):
            for j in range(2):
                print('\tcropping files (%d/%d)' % ((n-1)*4+k+1,np*4) , "     \r", end=' ')
                sys.stdout.flush()
                oy = i*dy
                ox = j*dx
                outfile = infile.replace('.pdf', '_%d.pdf' % k)
                files.append(outfile)
                cmd = ['pdfcrop', '--bbox', 
                    '%d %d %d %d' % (ox, oy, ox+dx, oy+dy),  
                    infile, outfile]
                #print cmd
                #subprocess.call(cmd)
                runcmd(cmd)
                k+=1
        # del pages
        os.unlink(infile)
    
    print("")
    currentdir = os.getcwd()

    os.chdir(tmpdir)    # -- goto tmp folder
    cmd = ['pdfjoin'] + files
    runcmd(cmd)

    joinedfile = files[-1].replace('.pdf','-joined.pdf')

    # printe le resultat avec ghostview (pour virer tout ce qui n'est pas visible)
    print('\ttrimming file with ghostview')
    outputfile = joinedfile.replace('.pdf','-trim-a6.pdf')
    cmd=['gs', '-dNOPAUSE', '-dBATCH', '-sDEVICE=pdfwrite',
        '-dCompatibilityLevel=1.4', '-dPDFSETTINGS="/ebook"', 
        '-sOutputFile=%s' % outputfile, 
        joinedfile]
    runcmd(cmd)

    os.chdir(currentdir) # -- back to initial folder
   
    # convert A6/landscape to A4/landscape
    print('\tconverting from A6 to A4/landscape')
    inputfile = os.path.join(tmpdir, outputfile)
    outputfile = fname.replace('.pdf','-1x1.pdf')
    cmd = ['pdfjam', '--outfile', outputfile, '--paper', 
        'a4paper', '--landscape', inputfile]
    runcmd(cmd)


if __name__=="__main__":

    if len(sys.argv)==1:
        print('usage: %s [files.pdf]' % sys.argv[0])
        sys.exit(1)

    for i in range(1,len(sys.argv)):
        fname = sys.argv[i]
        if os.path.splitext(fname)[1] != '.pdf':
            print('skipping non pdf file (%s)')
            continue
        
        print('processing', fname)
        # create tmpdir
        fnamef = os.path.abspath(fname)

        # create a tmp folder
        #tmpdir = os.path.join(os.path.dirname(fnamef), 'tmp')
        curdir = os.getcwd()
        tmpdir = os.path.abspath(os.path.basename(fnamef).replace('.pdf','.tmp').replace('.','_'))
        #print 'tmpdir =', tmpdir
        
        if not os.path.isdir(tmpdir):
            os.mkdir(tmpdir)

        try:
            splitpdf(fname, tmpdir)
        finally:
            print('removing tmp files.')
            os.chdir(curdir)
            if os.path.isdir(tmpdir):
                shutil.rmtree(tmpdir)
