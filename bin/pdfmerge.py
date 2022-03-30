#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# usage: pdfmerge.py *.pdf => creates "binder.pdf" in the current folder

# see https://stackoverflow.com/questions/2507766/merge-convert-multiple-pdf-files-into-one-pdf

import sys, subprocess

# print('sys.argv =',sys.argv)
if len(sys.argv)==1:
    print(f'usage: {sys.argv[0]} file1.pdf file2.pdf ...')
    sys.exit(1)

cmd = [ 'gs' ]  # uses ghostscript
cmd.append('-sDEVICE=pdfwrite')
# cmd.append('-dCompatibilityLevel=1.4 ')
cmd.append('-dPDFSETTINGS=/prepress')  # /prepress
cmd.append('-dNOPAUSE')
cmd.append('-dQUIET')
cmd.append('-dBATCH')
# cmd.append('-dDetectDuplicateImages')
# cmd.append('-dCompressFonts=true')
# cmd.append('-r150')
cmd.append('-sOutputFile=binder.pdf' )
cmd += sys.argv[1:]

print(cmd)

subprocess.call(cmd)











