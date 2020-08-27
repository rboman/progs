#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import sys
import os
import fnmatch
import re
import subprocess


def all_files(root,
              patterns='*',
              skips='*.git*;*build*',
              single_level=False,
              yield_folders=False):
    # self.checkPath(root)
    patterns = patterns.split(';')
    skips = skips.split(';')
    for path, subdirs, files in os.walk(root):
        # print('processing folder', path)
        if yield_folders:
            files.extend(subdirs)
        files.sort()
        for name in files:
            for pattern in patterns:
                if fnmatch.fnmatch(name, pattern):
                    fullname = os.path.join(path, name)
                    ok = True
                    for skip in skips:
                        if fnmatch.fnmatch(fullname, skip):
                            ok = False
                    if ok:
                        yield fullname
                        break
        if single_level:
            break


def main():

    # loop over all files and format them
    encs = {}
    for f in all_files(os.getcwd(), patterns='*.inc90.in',
                       skips='*.git*;*build*'):
        targetname = f.replace('.inc90.in','.inc.in')
        print(f, targetname)

        os.rename(f, targetname)
        # cmd = ['clang-format', "-style=file", "-i", f]
        # retcode = subprocess.call(cmd)
        # if retcode != 0:
        #     print(f'ERROR: retcode = {retcode}')
        #     break


if __name__ == "__main__":
    main()

