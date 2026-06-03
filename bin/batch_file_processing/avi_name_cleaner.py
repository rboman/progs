#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# petit script pas très utile, gardé pour mes mémoire...
#
# Nettoie les noms de fichiers avi/sub/idx/asf/mpg/wmv/mp4/mkv dans le répertoire courant.
# simple search/replace dans une série de fichiers
#
# usage: avi_name_cleaner.py [rename]
#

import os, os.path, sys


def clean(rename=False):
    """
    Clean filenames in current directory
    """
    # for file in os.listdir('.'):
    for root, dirs, files in os.walk('.'):
        for file in files:
            fullfile = os.path.join(root, file)
            if os.path.isfile(fullfile):
                base, ext = os.path.splitext(file)
                ext = ext.lower()
                if ext in ['.avi', '.sub', '.idx', '.srt', '.asf', '.mpg', '.wmv', '.mp4', '.mkv']:
                    base = base.replace('.', ' ')
                    base = base.replace('_', ' ')
                    base = base.replace('cd1', 'CD1')
                    base = base.replace('cd2', 'CD2')
                    # base = base.title()
                    newfile = base + ext
                    if file != newfile:
                        if rename:
                            print("renaming",
                                  os.rename(fullfile, os.path.join(root, newfile)))
                        print("'%s' => '%s'" % (file, newfile))


def main():
    rename = False
    for arg in sys.argv[1:]:
        if arg == "rename":
            rename = True
    clean(rename)


if __name__ == "__main__":
    main()

    print("\n[ENTER]")
    input()
