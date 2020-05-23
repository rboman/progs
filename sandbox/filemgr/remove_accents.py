#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# RB - (rem: utiliser un coding utf-8 sous Windows)

from __future__ import print_function
from builtins import object
import os
import os.path
import glob
import sys

codes = {
    'â': 'a',
    'à': 'a',
    'ä': 'a',
    'è': 'e',
    'ê': 'e',
    'ë': 'e',
    'é': 'e',
    'î': 'i',
    'ï': 'i',
    'ô': 'o',
    'ö': 'o',
    'ù': 'u',
    'û': 'u',
    'ü': 'u',
    'ç': 'c'
}


def convert(file, path):
    file2 = file
    for x1, x2 in list(codes.items()):
        file2 = file2.replace(x1, x2)
    if file2 != file:
        print('\trename "%s" to "%s"? [y/n]' % (file, file2), end=' ')
        c = getch()
        if c == 'y':
            try:
                os.rename(os.path.join(path, file), os.path.join(path, file2))
                print("DONE!")
            except:
                print("FAILED!")
        else:
            print("SKIPPED!")


class _Getch(object):
    """Gets a single character from standard input.  Does not echo to the screen."""

    def __init__(self):
        try:
            self.impl = _GetchWindows()
        except ImportError:
            self.impl = _GetchUnix()

    def __call__(self): return self.impl()


class _GetchUnix(object):
    def __init__(self):
        import tty
        import sys

    def __call__(self):
        import sys
        import tty
        import termios
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch


class _GetchWindows(object):
    def __init__(self):
        import msvcrt

    def __call__(self):
        import msvcrt
        return msvcrt.getch()


getch = _Getch()


def main():
    # convert files
    print("\nSTEP 1: Processing file names...\n")
    for dirs in sys.argv[1:]:
        for dir in glob.glob(dirs):
            if os.path.isdir(dir):
                for path, subdirs, files in os.walk(dir):
                    print('entering "%s"...' % path)
                    for file in files:
                        convert(file, path)
    # convert dirs
    print("\nSTEP 2: Processing directory names...\n")
    for dirs in sys.argv[1:]:
        for dir in glob.glob(dirs):
            if os.path.isdir(dir):
                for path, subdirs, files in os.walk(dir):
                    print('entering "%s"...' % path)
                    for file in subdirs:
                        convert(file, path)


if __name__ == "__main__":
    if len(sys.argv) == 1:
        print("\nusage: %s [dirnames]\n" % sys.argv[0])
        print(u"renomme les fichiers sans caractères non accentués\n")
        sys.exit()

    main()
