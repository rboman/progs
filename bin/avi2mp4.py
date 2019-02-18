#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# TODO: make it work on linux
# TODO: find ffmpeg more automatically

import sys, os, subprocess, glob

exeffmpeg = r'f:\local\ffmpeg\bin\ffmpeg.exe'
fps=30
quality= 18
"""
f:\local\ffmpeg\bin\ffmpeg.exe -i animation_trapez.avi -vf fps=30 -crf 18 -pix_fmt yuv420p video.mp4
"""

def convert(f):
    name,ext = os.path.splitext(f)
    #if ext.lower()!='.avi':
    #    print 'ignoring %s (not .avi)' % f
    #    return
    
    cmd = []
    cmd.append(exeffmpeg)    
    cmd.append('-y')
    cmd.extend(['-i', f])
    cmd.extend(['-c:v', 'libx264'])
    cmd.extend(['-crf', '%d' % quality])
    cmd.extend(['-pix_fmt', 'yuv420p'])
    # chain of 2 filters (crop & fps) delimited by a comma
    cmd.extend(['-vf', '[in]crop=trunc(iw/2)*2:trunc(ih/2)*2:0:0,fps=%d[out]'% fps ])
    #cmd.extend(['-vf', 'fps=%d'% fps])    
    outfile = name+".mp4"
    cmd.append(outfile)
    print cmd    
    retcode = subprocess.call(cmd)
    print "retcode =", retcode

if __name__=="__main__":
    if len(sys.argv)==1:
        print 'usage:\n\t%s [file1.avi] [file2.avi] ...' % sys.argv[0]
    else:
        for arg in sys.argv[1:]:
            for f in glob.glob(arg):
                print 'processing', f
                convert(f)

