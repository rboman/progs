#!/usr/bin/env python3
# -*- coding: utf-8 -*-


# a regarder: https://pypi.org/project/tinydb/

import os
import fnmatch

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

# https://stackoverflow.com/questions/1094841/get-human-readable-version-of-file-size
def sizeof_fmt(num, suffix="B"):
    for unit in ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"]:
        if abs(num) < 1024.0:
            return f"{num:3.1f}{unit}{suffix}"
        num /= 1024.0
    return f"{num:.1f}Yi{suffix}"

def main():


    # store all files in a DB

    # basedir = r"F:\Dropbox\Library"
    basedir = r"F:\Dropbox\Library\Science"
    # basedir = r"F:\Dropbox"

    tags = {}   # tag_name => files


    from pathlib import Path # https://docs.python.org/3/library/pathlib.html
    from datetime import datetime

    nfiles = 0
    totsize = 0
    for f in all_files(basedir, patterns='*.pdf',
                       skips='*.git*;*build*'):

        print(f)
        # print(f'\t {Path(f).stat()}')
        stat = Path(f).stat()
        size = stat.st_size # https://docs.python.org/3/library/os.html#os.stat_result
        print(f'\tsize = {size} ({sizeof_fmt(size)})')
        print(f'\tcreated = {stat.st_ctime} ({datetime.fromtimestamp(stat.st_ctime)})')
        print(f'\tmodified = {stat.st_mtime} ({datetime.fromtimestamp(stat.st_mtime)})')
        print(f'\taccessed = {stat.st_atime} ({datetime.fromtimestamp(stat.st_atime)})')

        # extract info:
        # https://realpython.com/pdf-python/#how-to-extract-document-information-from-a-pdf-in-python

        totsize += size
        nfiles+=1
        
    print(f'{nfiles} files processed.')
    print(f'total size: {sizeof_fmt(totsize)}.')


if __name__ == "__main__":
    main()

