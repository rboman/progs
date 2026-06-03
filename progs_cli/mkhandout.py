#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Script pour faire des handouts à partir de pdf de présentation.
# Utilise pdfjam (pdfnup) pour faire du 2x1 (2 pages par feuille, en portrait).

import sys, os, os.path


def handout(infile):
    _, ext = os.path.splitext(infile)
    if ext != ".pdf":
        raise Exception("file should be a PDF")

    cmd = r'pdfjam --a4paper --keepinfo --nup 1x2 --frame true ' \
        ' --scale 0.92 --no-landscape ' \
        ' --trim \'-0.2cm -0.2cm -0.2cm -0.2cm\' --delta \'10 10\' ' \
        ' --suffix 1x2 "%s"' % infile
    """
    cmd = r'pdfnup --a4paper --keepinfo --nup 2x2 ' \
           ' --scale 0.92 ' \
           ' --trim \'-0.2cm -0.2cm -0.2cm -0.2cm\' --delta \'10 10\' ' \
           ' --suffix 2x2 "%s"' % infile
    """
    print(cmd)
    os.system(cmd)


def main():
    print("running with python", sys.version)
    for f in sys.argv[1:]:
        handout(f)


if __name__ == "__main__":
    main()
