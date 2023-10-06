#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Merge PDF and Python scripts into a single pdf.
#
# usage:
#   pdfmerge.py report.pdf *.py => creates "report-binder.pdf" in the current folder
#
# my usage for PDE reports:
#   pdfmerge.py *final.pdf *.py
#
# requirements:
#   a2ps, gs (ghostscript) or pdfunite (apt install poppler-utils)
#
# see https://stackoverflow.com/questions/2507766/merge-convert-multiple-pdf-files-into-one-pdf

import os
import sys
import subprocess


def merge_gs(files, output='binder.pdf'):
    cmd = ['gs']  # uses ghostscript
    cmd.append('-sDEVICE=pdfwrite')
    # cmd.append('-dCompatibilityLevel=1.4 ')
    cmd.append('-dPDFSETTINGS=/prepress')  # /prepress
    cmd.append('-dNOPAUSE')
    cmd.append('-dQUIET')
    cmd.append('-dBATCH')
    # cmd.append('-dDetectDuplicateImages')
    # cmd.append('-dCompressFonts=true')
    # cmd.append('-r150')
    cmd.append(f'-sOutputFile={output}')
    cmd += files
    # print(cmd)
    with open(os.devnull, 'w') as FNULL:
        subprocess.call(cmd, stdout=FNULL, stderr=subprocess.STDOUT)


def merge_pdfunite(files, output='binder.pdf'):
    cmd = ['pdfunite']
    cmd.extend(files)
    cmd.append(output)
    # print(cmd)
    with open(os.devnull, 'w') as FNULL:
        subprocess.call(cmd, stdout=FNULL, stderr=subprocess.STDOUT)


def py2pdf(file):
    """ convert a python script into a pdf file
    """
    # convert to postscript with a2ps
    cmd = ["a2ps"]
    cmd.append('--output=code.ps')
    cmd.append('--pro=color')
    cmd.append('--line-numbers=5')
    cmd.append('--medium=A4')
    cmd.append('-1')  # 1 page / landscape
    cmd.append(f'--output={file}.ps')
    cmd.append(file)
    # print(cmd)
    # logf = os.devnull
    logf = "a2ps.log"
    with open(logf, 'w') as FNULL:
        subprocess.call(cmd, stdout=FNULL, stderr=subprocess.STDOUT)

    # ps to pdf with ghostscript
    cmd = ["gs"]
    cmd.append('-sDEVICE=pdfwrite')
    cmd.append('-g5950x8420')
    cmd.append('-dPDFFitPage')
    cmd.append(f'-o{file}.pdf')
    cmd.append(f'{file}.ps')
    # print(cmd)
    # logf = os.devnull
    logf = "gs.log"
    with open(logf, 'w') as FNULL:
        subprocess.call(cmd, stdout=FNULL, stderr=subprocess.STDOUT)

    os.remove(f'{file}.ps')

    return f'{file}.pdf'


if __name__ == '__main__':

    if len(sys.argv) == 1:
        print(f'usage: {sys.argv[0]} file1.[pdf|py] file2.[pdf|py] ...')
        sys.exit(1)

    firstpdf = ''

    files = []
    tmpfiles = []
    for f in sys.argv[1:]:
        ext = os.path.splitext(f)[1]
        if ext in ['.py', '.h', '.cpp', '.hpp']:
            # convert python files
            tmp = py2pdf(f)
            tmpfiles.append(tmp)
            files.append(tmp)
        elif ext == '.pdf':
            files.append(f)
            if not firstpdf:
                firstpdf = f

    # print(files)
    # print(tmpfiles)

    if firstpdf:
        outname = f"{os.path.splitext(firstpdf)[0]}-binder.pdf"
    else:
        outname = "binder.pdf"

    # merge_gs(files, outname)
    merge_pdfunite(files, outname)

    # clean temporary files
    for f in tmpfiles:
        os.remove(f)
