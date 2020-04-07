#!/usr/bin/env python
# -*- coding: utf8 -*-
# test encoding: à-é-è-ô-ï-€

from __future__ import print_function
import sys, os
import fnmatch, re
import subprocess


def all_files(root,
              patterns='*',
              skips='*.svn*;*.git*',
              single_level=False,
              yield_folders=True):
    #self.checkPath(root)
    patterns = patterns.split(';')
    skips = skips.split(';')
    for path, subdirs, files in os.walk(root):
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


def getencoding_file(f):  # uses "file" linux command
    cmd = 'file -bi "%s"' % f
    #print cmd
    try:
        output = subprocess.check_output(
            cmd, stderr=subprocess.STDOUT, shell=True)
    except OSError:
        return '"file" cmd not found'
    m = re.match(r'.+charset=(.+)', output)
    if m and len(m.groups()) > 0:
        return m.group(1)
    else:
        return "ERROR: " + output


def getencoding_chardet(f):  # uses "chardet" module
    import chardet
    with open(f, 'r') as file:
        content = file.read()
    out = chardet.detect(content)
    #print out
    return out['encoding']


def getnonascii(fichier, enc):
    noascii = set()
    f = open(fichier)
    bytes = f.read()
    f.close()
    #print type(bytes)
    #print bytes
    utxt = bytes.decode(
        enc)  # le resultat de decode est tjs de l'unicode (utf8)
    #print utxt
    for uc in utxt:
        try:
            ascii = uc.encode('ascii')
        except:
            noascii.add(uc)
            #print 'non ascii:', uc

    return noascii


def main():
    print("sys.stdout.encoding:", sys.stdout.encoding)  # cp850 sous windows 10 / UTF-8 sous linux
    print("sys.getfilesystemencoding():", sys.getfilesystemencoding(
    ))  # mbcs sous windows 10 / UTF-8 sous linux

    if 1:
        # loop over all files and try to guess encoding...
        encs = {}
        for f in all_files(os.getcwd(), patterns='*.py;*.h;*.cpp;*.inl;*.i;*.hpp'):
            enc = getencoding_file(f)
            #enc = getencoding_chardet(f) # marche moins bien
            if enc not in encs:
                encs[enc] = []
            encs[enc].append(f)

        # display results
        for enc in encs:
            if enc in ['binary', 'us-ascii', None, 'ascii']:
                continue
            print(enc, 'encoding:')
            for f in encs[enc]:
                print('\t', f, end=' ')
                # display non-ASCII chars
                noascii = getnonascii(f, enc)
                print(' non ASCII=(', end=' ')
                for c in noascii:
                    print(c, end=' ')
                print(')')


if __name__ == "__main__":
    main()