#!/usr/bin/env python
# -*- coding: utf-8 -*-
# test encoding: à-é-è-ô-ï-€

from __future__ import print_function
import sys, os


def main():
    print("sys.stdout.encoding:", sys.stdout.encoding)  # cp850 sous windows 10 / UTF-8 sous linux
    print("sys.getfilesystemencoding():", sys.getfilesystemencoding(
    ))  # mbcs sous windows 10 / UTF-8 sous linux

    fichier = 'cp1252.txt'
    f = open(fichier)
    bytes = f.read()
    f.close()
    print(type(bytes))
    print(bytes)
    txt = bytes.decode(
        'cp1252')  # le resultat de decode est tjs de l'unicode (utf8)
    print(txt)  # print de l'utf8 brutalement - ca marche sous linux pq tout est en utf8...
    #txtascii = txt.encode('ascii')


if __name__ == "__main__":
    main()