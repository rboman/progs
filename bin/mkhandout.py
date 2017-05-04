#!/usr/bin/env python
# -*- coding: latin-1; -*-

import sys, os, os.path

def handout(infile):

    file, ext = os.path.splitext(infile)
    if ext!=".pdf":
        raise Exception("file should be a PDF")
    outfile=file+"-handout.pdf"
    cmd='pdfnup --a4paper --keepinfo --nup 1x2 --frame true --scale 0.92 --no-landscape --outfile "%s" "%s"' % (outfile, infile)
    print cmd
    os.system(cmd)

if __name__=="__main__":
    if len(sys.argv)!=2:
        print "usage: %s file.pdf" % sys.argv[0]
    else:
        handout(sys.argv[1])

