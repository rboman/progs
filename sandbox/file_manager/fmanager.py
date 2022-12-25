#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# futur utilitaire de gestion de ma dropbox
#
# pourrait etre utile à terme pour faire des stats et du bulk-processing 
# de fichiers
# (rename / conversion unicode / tags / nettoyages divers / traitement via GUI)

# a regarder: https://pypi.org/project/tinydb/

import os
import fnmatch
import re

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

def sizeof_fmt(num, suffix="B"):
    """convert file size into a human-readble string
    see https://stackoverflow.com/questions/1094841/get-human-readable-version-of-file-size
    """
    for unit in ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"]:
        if abs(num) < 1024.0:
            return f"{num:3.1f}{unit}{suffix}"
        num /= 1024.0
    return f"{num:.1f}Yi{suffix}"



def display_files_info(basedir):
    """loop of pdf files and print info of each file
    """

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


def retrieve_files(basedir):

    files = []
    for f in all_files(basedir, patterns='*.pdf',
                       skips='*.git*;*build*'):
        file = {}
        file['fullpath'] = f
        file['folder'] = os.path.dirname(f)
        file['name'] = os.path.basename(f)
        _, file['ext'] = os.path.splitext(file['name'])
        m = re.findall('\[(.+?)\]', file['name'])
        # https://docs.python.org/3/howto/regex.html#greedy-versus-non-greedy
        file['tags'] = m
        # if m:
        #     print(f'\ttags:{m.groups()}')
        #     file['tags'] = list(m.groups())
        # else:
        #     file['tags'] = []
        files.append(file)
    return files

if __name__ == "__main__":

    basedir = r"F:\Dropbox\Library\ULg"
    # basedir = r"F:\Dropbox\Library\Science"
    basedir = r"F:\Dropbox"

    # display_files_info(r"F:\Dropbox\Library\Science")

    print("retrieving file info...")
    files = retrieve_files(basedir)
    print(f"{len(files)} files processed.")

    # for f in files:
    #     print(f["name"], '\ttags:', f['tags'])


    tags = [] # unique tags

    bytags = {}

    for f in files:
        for tag in f['tags']:
            if not tag in tags:
                tags.append(tag)
                bytags[tag] = []
            bytags[tag].append( f )
    
    print('\ntags:', tags)

    print('\tfiles by tag:')
    for tag, fs in bytags.items():
        print(f'[{tag}]:')
        for f in fs:
            try:
                print('\t', f['fullpath'])
            except:
                print('\tENCODING ERROR:', f['fullpath'].encode('ascii', 'ignore'))



