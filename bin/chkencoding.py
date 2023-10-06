#!/usr/bin/env python3
# -*- coding: utf8 -*-
# test encoding: à-é-è-ô-ï-€
#
# This script changes the encoding of the source files in the current
# folder (and sub-folders) to UTF-8.

import sys
import os
import fnmatch
import re
import subprocess
import platform

file_types = '*.py;*.pyw;*.h;*.c;*.cpp;*.inl;*.i;*.hpp;*.txt;*.swg;*.for;*.f;*.f90;*.cxx;*.hxx'


def all_files(root,
              patterns='*',
              skips='*.svn*;*.git*;*build*',
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


def getencoding_file(f):
    """returns text encoding type of file "f"
    (with the "file" linux command)
    """
    if 'Windows' in platform.uname():
        cmd = r'C:\msys64\usr\bin\file -bi "%s"' % f
    else:
        cmd = 'file -bi "%s"' % f

    # run "file"
    try:
        output = subprocess.check_output(
            cmd, stderr=subprocess.STDOUT, shell=True)
    except OSError:
        return '"file" cmd not found'

    # parse output
    m = re.match(r'.+charset=(.+)', output.decode())
    if m and len(m.groups()) > 0:
        return m.group(1)
    else:
        return "ERROR: " + output


def getencoding_chardet(f):
    """returns text encoding type of file "f"
    (with the "chardet" python module)
    makes a lot of mistakes 
    """
    import chardet
    with open(f, 'r') as file:
        content = file.read()
    out = chardet.detect(content)
    return out['encoding']


def getnonascii(f, enc):
    """returns a set of non-ascii characters
    """
    noascii = set()
    f = open(f)
    bytes = f.read()
    f.close()
    utxt = bytes.decode(enc)
    # note: le résultat de "decode" est tjs de l'unicode (utf8)

    for uc in utxt:
        try:
            ascii = uc.encode('ascii')
        except:
            noascii.add(uc)

    return noascii


def main():

    # display info about terminal and file system encoding
    #   stdout: cp850 sous windows 10 / UTF-8 sous linux
    #   filesystem: mbcs sous windows 10 / UTF-8 sous linux
    print("sys.stdout.encoding:", sys.stdout.encoding)
    print("sys.getfilesystemencoding():", sys.getfilesystemencoding())

    # loop over all files and try to guess text encoding...
    encs = {}  # dict "encoding" => list of files
    for f in all_files(os.getcwd(), patterns=file_types):
        print('{}..'.format(f), end=' ')
        # utilise "file" - marche très bien
        enc = getencoding_file(f)
        # marche (beaucoup) moins bien:
        #   détecte du latin-1 pour des fichiers convertis en utf-8!
        # enc = getencoding_chardet(f)
        print('{}.'.format(enc))

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
            try:
                noascii = getnonascii(f, enc)
            except:
                noascii = ['?']
            print(' non ASCII=(', end=' ')
            for c in noascii:
                try:
                    print(c, end=' ')
                except:
                    print('?', end=' ')

            print(')')

    # convert files
    #   rem: ISO-8859-1 == latin-1
    if 1:
        for enc in encs:
            for f in encs[enc]:
                # convert to utf-8
                if enc in ['binary', 'us-ascii', None, 'ascii', 'utf-8']:
                    continue
                # convertit les fichiers récalcitrants
                #if enc=='unknown-8bit': enc='ISO-8859-1'
                #if enc=='unknown-8bit': enc='CP437'
                if enc == 'unknown-8bit':
                    enc = 'CP1252'  # Windows1252 assumed
                try:
                    with open(f, "rb") as source:
                        content = str(source.read(), enc).encode('utf-8')
                        #content = unicode(source.read(), enc, errors='ignore').encode('utf-8', errors='ignore')
                        with open(f, "wb") as target:
                            target.write(content)
                except:
                    print('Problem while reading/writing', f)


if __name__ == "__main__":

    # parse cmd-line arguments
    import argparse
    parser = argparse.ArgumentParser(description='UTF-8 converter.')

    main()
