#!/usr/bin/env python3

import sys


def clean(file):
    with open(file+'.ok', 'w') as fout:
        with open(file, 'r', encoding='ascii', errors='ignore') as fin:
            for l in fin.readlines():
                fout.write(l)


if __name__ == "__main__":
    clean(sys.argv[1])
