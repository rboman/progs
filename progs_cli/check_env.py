#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Petit script pour vérifier les variables d'environnement PATH, LD_LIBRARY_PATH, PYTHONPATH, etc.
# Affiche les entrées dupliquées et les chemins invalides.

#


import os


def find_duplicates():
    print("duplicate entries in env vars:")
    for var in list(os.environ.keys()):
        # print var
        parts = os.environ[var].split(":")
        # print parts
        dupes = [x for n, x in enumerate(parts) if x in parts[:n]]
        if len(dupes):
            print(" .", var, ": ", dupes)


def find_invalid_paths():
    print("\ninvalid paths/files in env vars:")
    for var in list(os.environ.keys()):
        parts = os.environ[var].split(":")
        for p in parts:
            if os.sep in p:
                if not os.path.isdir(p) and not os.path.isfile(p):
                    print(" .", var, ": ", p, " not found")


def main():
    find_duplicates()
    find_invalid_paths()


if __name__ == "__main__":
    main()
